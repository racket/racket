/*
  MzScheme
  Copyright (c) 2004-2007 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  Originally based on:
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* MzScheme function prototypes */
/* Macros generally shouldn't go in this file; it is used both to
   prototype functions, and as a parsing source for
   declaring scheme_extension_table */

/* The scheme_extension_table "parser" is picky; don't leave a space
   between a function name and it's opening parameter parenthesis. */

/* After this START tag, all comments should start & end on same line */

/* START */

/*========================================================================*/
/*                       setjmpup (continuations)                         */
/*========================================================================*/

MZ_EXTERN void scheme_init_jmpup_buf(Scheme_Jumpup_Buf *b);
MZ_EXTERN int scheme_setjmpup_relative(Scheme_Jumpup_Buf *b, void *base,
				       void * volatile start, struct Scheme_Cont *cont);
MZ_EXTERN void scheme_longjmpup(Scheme_Jumpup_Buf *b);
MZ_EXTERN void scheme_reset_jmpup_buf(Scheme_Jumpup_Buf *b);

#ifdef USE_MZ_SETJMP
MZ_EXTERN int scheme_mz_setjmp(mz_pre_jmp_buf b);
MZ_EXTERN void scheme_mz_longjmp(mz_pre_jmp_buf b, int v);
#endif

MZ_EXTERN void scheme_clear_escape(void);

MZ_EXTERN Scheme_Jumpup_Buf_Holder *scheme_new_jmpupbuf_holder(void);

/*========================================================================*/
/*                                parameters                              */
/*========================================================================*/

MZ_EXTERN Scheme_Config *scheme_current_config(void);
MZ_EXTERN Scheme_Config *scheme_extend_config(Scheme_Config *c, int pos, Scheme_Object *init_val);
MZ_EXTERN void scheme_install_config(Scheme_Config *);

MZ_EXTERN Scheme_Object *scheme_get_param(Scheme_Config *c, int pos);
MZ_EXTERN void scheme_set_param(Scheme_Config *c, int pos, Scheme_Object *o);

MZ_EXTERN Scheme_Object *scheme_get_thread_param(Scheme_Config *c, Scheme_Thread_Cell_Table *cells, int pos);
MZ_EXTERN void scheme_set_thread_param(Scheme_Config *c, Scheme_Thread_Cell_Table *cells, int pos, Scheme_Object *o);

MZ_EXTERN Scheme_Env *scheme_get_env(Scheme_Config *config);

MZ_EXTERN Scheme_Thread_Cell_Table *scheme_inherit_cells(Scheme_Thread_Cell_Table *cells);

MZ_EXTERN Scheme_Object *scheme_current_break_cell();

/*========================================================================*/
/*                                threads                                 */
/*========================================================================*/

#ifndef LINK_EXTENSIONS_BY_TABLE
MZ_EXTERN Scheme_Thread *scheme_current_thread;
MZ_EXTERN volatile int scheme_fuel_counter;
#else
MZ_EXTERN Scheme_Thread **scheme_current_thread_ptr;
MZ_EXTERN volatile int *scheme_fuel_counter_ptr;
#endif

MZ_EXTERN Scheme_Thread *scheme_get_current_thread();

MZ_EXTERN void scheme_start_atomic(void);
MZ_EXTERN void scheme_end_atomic(void);
MZ_EXTERN void scheme_end_atomic_no_swap(void);

MZ_EXTERN void scheme_out_of_fuel(void);

MZ_EXTERN Scheme_Object *scheme_thread(Scheme_Object *thunk);
MZ_EXTERN Scheme_Object *scheme_thread_w_details(Scheme_Object *thunk, 
						 Scheme_Config *init_config,
						 Scheme_Thread_Cell_Table *copy_from,
						 Scheme_Object *break_cell,
						 Scheme_Custodian *owning_custodian, 
						 int suspend_to_kill);
MZ_EXTERN void scheme_kill_thread(Scheme_Thread *p);
MZ_EXTERN void scheme_break_thread(Scheme_Thread *p);

MZ_EXTERN void scheme_thread_block(float sleep_time);
MZ_EXTERN void scheme_thread_block_enable_break(float sleep_time, int enable);
MZ_EXTERN void scheme_swap_thread(Scheme_Thread *process);
MZ_EXTERN void scheme_making_progress();

MZ_EXTERN void scheme_weak_suspend_thread(Scheme_Thread *p);
MZ_EXTERN void scheme_weak_resume_thread(Scheme_Thread *p);

MZ_EXTERN int scheme_block_until(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun, Scheme_Object *, float);
MZ_EXTERN int scheme_block_until_enable_break(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun, Scheme_Object *, 
					      float, int enable);
MZ_EXTERN int scheme_block_until_unless(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun fdf,
					Scheme_Object *data, float delay, 
					Scheme_Object *unless,
					int enable_break);

MZ_EXTERN void scheme_wait_input_allowed(Scheme_Input_Port *port, int nonblock);
XFORM_NONGCING MZ_EXTERN int scheme_unless_ready(Scheme_Object *unless);

MZ_EXTERN int scheme_in_main_thread(void);

MZ_EXTERN void scheme_cancel_sleep(void);

MZ_EXTERN Scheme_Object *scheme_make_thread_cell(Scheme_Object *def_val, int inherited);
MZ_EXTERN Scheme_Object *scheme_thread_cell_get(Scheme_Object *cell, Scheme_Thread_Cell_Table *cells);
MZ_EXTERN void scheme_thread_cell_set(Scheme_Object *cell, Scheme_Thread_Cell_Table *cells, Scheme_Object *v);

MZ_EXTERN int scheme_tls_allocate();
MZ_EXTERN void scheme_tls_set(int pos, void *v);
MZ_EXTERN void *scheme_tls_get(int pos);

MZ_EXTERN Scheme_Custodian *scheme_make_custodian(Scheme_Custodian *);
MZ_EXTERN Scheme_Custodian_Reference *scheme_add_managed(Scheme_Custodian *m, Scheme_Object *o,
							 Scheme_Close_Custodian_Client *f, void *data,
							 int strong);
MZ_EXTERN void scheme_custodian_check_available(Scheme_Custodian *m, const char *who, const char *what);
MZ_EXTERN void scheme_remove_managed(Scheme_Custodian_Reference *m, Scheme_Object *o);
MZ_EXTERN void scheme_close_managed(Scheme_Custodian *m);
MZ_EXTERN void scheme_schedule_custodian_close(Scheme_Custodian *c);
MZ_EXTERN void scheme_add_custodian_extractor(Scheme_Type t, Scheme_Custodian_Extractor e);

MZ_EXTERN void scheme_add_atexit_closer(Scheme_Exit_Closer_Func f);

MZ_EXTERN void scheme_add_evt(Scheme_Type type,
				   Scheme_Ready_Fun ready,
				   Scheme_Needs_Wakeup_Fun wakeup,
				   Scheme_Sync_Filter_Fun filter,
				   int can_redirect);
MZ_EXTERN void scheme_add_evt_through_sema(Scheme_Type type,
					    Scheme_Sync_Sema_Fun sema,
					    Scheme_Sync_Filter_Fun filter);
MZ_EXTERN int scheme_is_evt(Scheme_Object *o);
MZ_EXTERN Scheme_Object *scheme_sync(int argc, Scheme_Object *argv[]);
MZ_EXTERN Scheme_Object *scheme_sync_enable_break(int argc, Scheme_Object *argv[]);
MZ_EXTERN Scheme_Object *scheme_sync_timeout(int argc, Scheme_Object *argv[]);

MZ_EXTERN Scheme_Object *scheme_make_evt_set(int argc, Scheme_Object **argv);

MZ_EXTERN void scheme_add_swap_callback(Scheme_Closure_Func f, Scheme_Object *data);

MZ_EXTERN Scheme_Object *scheme_call_enable_break(Scheme_Prim *prim, int argc, Scheme_Object *argv[]);
MZ_EXTERN int scheme_close_should_force_port_closed();

MZ_EXTERN void scheme_push_kill_action(Scheme_Kill_Action_Func f, void *d);
MZ_EXTERN void scheme_pop_kill_action();

MZ_EXTERN void scheme_set_can_break(int on);
MZ_EXTERN void scheme_push_break_enable(Scheme_Cont_Frame_Data *cframe, int on, int pre_check);
MZ_EXTERN void scheme_pop_break_enable(Scheme_Cont_Frame_Data *cframe, int post_check);

/*========================================================================*/
/*                              error handling                            */
/*========================================================================*/

MZ_EXTERN void scheme_signal_error(const char *msg, ...);
MZ_EXTERN void scheme_raise_exn(int exnid, ...);
MZ_EXTERN void scheme_warning(char *msg, ...);

MZ_EXTERN void scheme_raise(Scheme_Object *exn);

MZ_EXTERN void scheme_wrong_count(const char *name, int minc, int maxc,
				  int argc, Scheme_Object **argv);
MZ_EXTERN void scheme_wrong_count_m(const char *name, int minc, int maxc,
				    int argc, Scheme_Object **argv,
				    int is_method);
MZ_EXTERN void scheme_case_lambda_wrong_count(const char *name, int argc,
					      Scheme_Object **argv, int is_method, int count, ...);
MZ_EXTERN void scheme_wrong_type(const char *name, const char *expected,
				 int which, int argc,
				 Scheme_Object **argv);
MZ_EXTERN void scheme_wrong_field_type(Scheme_Object *c_name,
				       const char *expected,
				       Scheme_Object *o);
MZ_EXTERN void scheme_arg_mismatch(const char *name, const char *msg, Scheme_Object *o);
MZ_EXTERN void scheme_wrong_return_arity(const char *where,
					 int expected, int got,
					 Scheme_Object **argv,
					 const char *context_detail, ...);
MZ_EXTERN void scheme_unbound_global(Scheme_Bucket *b);

MZ_EXTERN Scheme_Object *scheme_dynamic_wind(void (*pre)(void *),
					     Scheme_Object *(* volatile act)(void *),
					     void (* volatile post)(void *),
					     Scheme_Object *(*jmp_handler)(void *),
					     void * volatile data);

/*========================================================================*/
/*                                 types                                  */
/*========================================================================*/

MZ_EXTERN Scheme_Type scheme_make_type(const char *name);

MZ_EXTERN char *scheme_get_type_name(Scheme_Type type);

/*========================================================================*/
/*                              constants                                 */
/*========================================================================*/

MZ_EXTERN Scheme_Object scheme_eof[1];
MZ_EXTERN Scheme_Object *scheme_make_eof(void);
MZ_EXTERN Scheme_Object scheme_null[1];
MZ_EXTERN Scheme_Object *scheme_make_null(void);
MZ_EXTERN Scheme_Object scheme_true[1];
MZ_EXTERN Scheme_Object *scheme_make_true(void);
MZ_EXTERN Scheme_Object scheme_false[1];
MZ_EXTERN Scheme_Object *scheme_make_false(void);
MZ_EXTERN Scheme_Object scheme_void[1];
MZ_EXTERN Scheme_Object *scheme_make_void(void);
MZ_EXTERN Scheme_Object scheme_undefined[1];
MZ_EXTERN Scheme_Object *scheme_tail_call_waiting;
MZ_EXTERN Scheme_Object *scheme_multiple_values;

MZ_EXTERN unsigned short *scheme_uchar_table[];
MZ_EXTERN unsigned char *scheme_uchar_cases_table[];
MZ_EXTERN unsigned char *scheme_uchar_cats_table[];
MZ_EXTERN int scheme_uchar_ups[];
MZ_EXTERN int scheme_uchar_downs[];
MZ_EXTERN int scheme_uchar_titles[];
MZ_EXTERN int scheme_uchar_folds[];
MZ_EXTERN unsigned char scheme_uchar_combining_classes[];

/*========================================================================*/
/*                              evaluation                                */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_with_prompt(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_multi_with_prompt(Scheme_Object *obj, Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env);
MZ_EXTERN Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_apply_multi(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_apply_no_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_apply_multi_no_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_apply_to_list(Scheme_Object *rator, Scheme_Object *argss);
MZ_EXTERN Scheme_Object *scheme_apply_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_apply_multi_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *_scheme_apply_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *_scheme_apply_multi_with_prompt(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
MZ_EXTERN Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int all);
MZ_EXTERN Scheme_Object *scheme_eval_string_with_prompt(const char *str, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_string_multi_with_prompt(const char *str, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_string_all_with_prompt(const char *str, Scheme_Env *env, int all);

MZ_EXTERN Scheme_Object *_scheme_apply_known_prim_closure(Scheme_Object *rator, int argc,
							  Scheme_Object **argv);
MZ_EXTERN Scheme_Object *_scheme_apply_known_prim_closure_multi(Scheme_Object *rator, int argc,
								Scheme_Object **argv);
MZ_EXTERN Scheme_Object *_scheme_apply_prim_closure(Scheme_Object *rator, int argc,
						    Scheme_Object **argv);
MZ_EXTERN Scheme_Object *_scheme_apply_prim_closure_multi(Scheme_Object *rator, int argc,
							  Scheme_Object **argv);

MZ_EXTERN Scheme_Object *scheme_call_with_prompt(Scheme_Closed_Prim f, void *data);
MZ_EXTERN Scheme_Object *scheme_call_with_prompt_multi(Scheme_Closed_Prim f, void *data);
MZ_EXTERN Scheme_Object *_scheme_call_with_prompt(Scheme_Closed_Prim f, void *data);
MZ_EXTERN Scheme_Object *_scheme_call_with_prompt_multi(Scheme_Closed_Prim f, void *data);

MZ_EXTERN Scheme_Object *scheme_values(int c, Scheme_Object **v);

MZ_EXTERN Scheme_Object *scheme_check_one_value(Scheme_Object *v);

/* Tail calls - only use these when you're writing new functions/syntax */
MZ_EXTERN Scheme_Object *scheme_tail_apply(Scheme_Object *f, int n, Scheme_Object **arg);
MZ_EXTERN Scheme_Object *scheme_tail_apply_no_copy(Scheme_Object *f, int n, Scheme_Object **arg);
MZ_EXTERN Scheme_Object *scheme_tail_apply_to_list(Scheme_Object *f, Scheme_Object *l);

MZ_EXTERN Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj);

MZ_EXTERN void scheme_set_tail_buffer_size(int s);
MZ_EXTERN Scheme_Object *scheme_force_value(Scheme_Object *);
MZ_EXTERN Scheme_Object *scheme_force_one_value(Scheme_Object *);

MZ_EXTERN MZ_MARK_STACK_TYPE scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val);
MZ_EXTERN void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *);
MZ_EXTERN void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *);
MZ_EXTERN void scheme_temp_dec_mark_depth();
MZ_EXTERN void scheme_temp_inc_mark_depth();

MZ_EXTERN Scheme_Object *scheme_current_continuation_marks(Scheme_Object *prompt_tag);
MZ_EXTERN Scheme_Object *scheme_extract_one_cc_mark(Scheme_Object *mark_set, 
						    Scheme_Object *key);
MZ_EXTERN Scheme_Object *scheme_extract_one_cc_mark_to_tag(Scheme_Object *mark_set, 
                                                           Scheme_Object *key,
                                                           Scheme_Object *prompt_tag);

/* Internal */
MZ_EXTERN Scheme_Object *scheme_do_eval(Scheme_Object *obj, int _num_rands, Scheme_Object **rands, int val);

MZ_EXTERN Scheme_Object *scheme_eval_compiled_stx_string(Scheme_Object *expr, Scheme_Env *env,
							 long shift, Scheme_Object *modidx);
MZ_EXTERN Scheme_Object *scheme_load_compiled_stx_string(const char *str, long len);
MZ_EXTERN Scheme_Object *scheme_compiled_stx_symbol(Scheme_Object *stx);

MZ_EXTERN Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);
MZ_EXTERN Scheme_Object *scheme_eval_compiled_sized_string_with_magic(const char *str, int len, Scheme_Env *env, 
								      Scheme_Object *magic_symbol, Scheme_Object *magic_val,
								      int multi_ok);

/*========================================================================*/
/*                           memory management                            */
/*========================================================================*/

/* The core allocator functions depend on the GC. Macros in scheme.h */
/*  map to the apporpriate core allocation function. */

#ifndef SCHEME_NO_GC
# ifndef SCHEME_NO_GC_PROTO
MZ_EXTERN void *GC_malloc(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_atomic(size_t size_in_bytes);
#  ifdef MZ_PRECISE_GC
MZ_EXTERN void *GC_malloc_one_tagged(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
MZ_EXTERN void *scheme_malloc_uncollectable(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_array_tagged(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_allow_interior(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_atomic_allow_interior(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_tagged_allow_interior(size_t size_in_bytes);
#  else
MZ_EXTERN void *GC_malloc_stubborn(size_t size_in_bytes);
MZ_EXTERN void *GC_malloc_uncollectable(size_t size_in_bytes);
#  endif
# endif
#endif

MZ_EXTERN void *scheme_malloc_eternal(size_t n);
MZ_EXTERN void scheme_end_stubborn_change(void *p);

MZ_EXTERN void *scheme_calloc(size_t num, size_t size);

MZ_EXTERN char *scheme_strdup(const char *str);
MZ_EXTERN char *scheme_strdup_eternal(const char *str);

MZ_EXTERN void *scheme_malloc_fail_ok(void *(*f)(size_t), size_t);

#ifndef MZ_PRECISE_GC
MZ_EXTERN void scheme_weak_reference(void **p);
MZ_EXTERN void scheme_weak_reference_indirect(void **p, void *v);
MZ_EXTERN void scheme_unweak_reference(void **p);
#endif
MZ_EXTERN void scheme_add_finalizer(void *p, void (*f)(void *p, void *data), void *data);
MZ_EXTERN void scheme_add_finalizer_once(void *p, void (*f)(void *p, void *data), void *data);
MZ_EXTERN void scheme_subtract_finalizer(void *p, void (*f)(void *p, void *data), void *data);
MZ_EXTERN void scheme_add_scheme_finalizer(void *p, void (*f)(void *p, void *data), void *data);
MZ_EXTERN void scheme_add_scheme_finalizer_once(void *p, void (*f)(void *p, void *data), void *data);
MZ_EXTERN void scheme_register_finalizer(void *p,
					 void (*f)(void *p, void *data), void *data,
					 void (**oldf)(void *p, void *data),
					 void **olddata);
MZ_EXTERN void scheme_remove_all_finalization(void *p);

MZ_EXTERN void scheme_dont_gc_ptr(void *p);
MZ_EXTERN void scheme_gc_ptr_ok(void *p);

MZ_EXTERN void scheme_collect_garbage(void);

#ifdef MZ_PRECISE_GC
MZ_EXTERN void **GC_variable_stack;
MZ_EXTERN void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup,
				      int is_constant_size, int is_atomic);
MZ_EXTERN void *GC_resolve(void *p);
MZ_EXTERN void GC_mark(const void *p);
MZ_EXTERN void GC_fixup(void *p);
#endif

MZ_EXTERN void **scheme_malloc_immobile_box(void *p);
MZ_EXTERN void scheme_free_immobile_box(void **b);

/*========================================================================*/
/*                             hash tables                                */
/*========================================================================*/

MZ_EXTERN Scheme_Bucket_Table *scheme_make_bucket_table(int size_hint, int type);
MZ_EXTERN void scheme_add_to_table(Scheme_Bucket_Table *table, const char *key, void *val, int);
MZ_EXTERN void scheme_change_in_table(Scheme_Bucket_Table *table, const char *key, void *new_val);
MZ_EXTERN void *scheme_lookup_in_table(Scheme_Bucket_Table *table, const char *key);
MZ_EXTERN Scheme_Bucket *scheme_bucket_from_table(Scheme_Bucket_Table *table, const char *key);
MZ_EXTERN int scheme_bucket_table_equal(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2);
MZ_EXTERN Scheme_Bucket_Table *scheme_clone_bucket_table(Scheme_Bucket_Table *bt);

MZ_EXTERN Scheme_Hash_Table *scheme_make_hash_table(int type);
MZ_EXTERN Scheme_Hash_Table *scheme_make_hash_table_equal();
MZ_EXTERN void scheme_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val);
MZ_EXTERN Scheme_Object *scheme_hash_get(Scheme_Hash_Table *table, Scheme_Object *key);
XFORM_NONGCING MZ_EXTERN Scheme_Object *scheme_eq_hash_get(Scheme_Hash_Table *table, Scheme_Object *key);
MZ_EXTERN void scheme_hash_set_atomic(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val);
MZ_EXTERN Scheme_Object *scheme_hash_get_atomic(Scheme_Hash_Table *table, Scheme_Object *key);
MZ_EXTERN int scheme_hash_table_equal(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2);
MZ_EXTERN int scheme_is_hash_table_equal(Scheme_Object *o);
MZ_EXTERN Scheme_Hash_Table *scheme_clone_hash_table(Scheme_Hash_Table *bt);

/*========================================================================*/
/*                   basic Scheme value constructors                      */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_make_prim(Scheme_Prim *prim);
MZ_EXTERN Scheme_Object *scheme_make_noneternal_prim(Scheme_Prim *prim);
MZ_EXTERN Scheme_Object *scheme_make_prim_w_arity(Scheme_Prim *prim, const char *name,
					mzshort mina, mzshort maxa);
MZ_EXTERN Scheme_Object *scheme_make_folding_prim(Scheme_Prim *prim,
					const char *name,
					mzshort mina, mzshort maxa,
					short functional);
MZ_EXTERN Scheme_Object *scheme_make_noncm_prim(Scheme_Prim *prim,
						const char *name,
						mzshort mina, mzshort maxa);
MZ_EXTERN Scheme_Object *scheme_make_noneternal_prim_w_arity(Scheme_Prim *prim,
						   const char *name,
						   mzshort mina, mzshort maxa);
MZ_EXTERN Scheme_Object *scheme_make_prim_w_everything(Scheme_Prim *fun, int eternal,
						       const char *name,
						       mzshort mina, mzshort maxa,
						       int folding,
						       mzshort minr, mzshort maxr);

MZ_EXTERN Scheme_Object *scheme_make_prim_closure_w_arity(Scheme_Primitive_Closure_Proc *prim,
							  int size, Scheme_Object **vals,
							  const char *name,
							  mzshort mina, mzshort maxa);
MZ_EXTERN Scheme_Object *scheme_make_folding_prim_closure(Scheme_Primitive_Closure_Proc *prim,
							  int size, Scheme_Object **vals,
							  const char *name,
							  mzshort mina, mzshort maxa,
							  short functional);

MZ_EXTERN Scheme_Object *scheme_make_closed_prim(Scheme_Closed_Prim *prim, void *data);
MZ_EXTERN Scheme_Object *scheme_make_closed_prim_w_arity(Scheme_Closed_Prim *prim,
							 void *data, const char *name,
							 mzshort mina, mzshort maxa);
MZ_EXTERN Scheme_Object *scheme_make_folding_closed_prim(Scheme_Closed_Prim *prim,
							 void *data, const char *name,
							 mzshort mina, mzshort maxa,
							 short functional);
MZ_EXTERN Scheme_Object *scheme_make_closed_prim_w_everything(Scheme_Closed_Prim *fun,
							      void *data,
							      const char *name,
							      mzshort mina, mzshort maxa,
							      short folding,
							      mzshort minr, mzshort maxr);

MZ_EXTERN void scheme_prim_is_method(Scheme_Object *o);

MZ_EXTERN Scheme_Object *scheme_make_pair(Scheme_Object *car, Scheme_Object *cdr);
MZ_EXTERN Scheme_Object *scheme_make_immutable_pair(Scheme_Object *car, Scheme_Object *cdr);

MZ_EXTERN Scheme_Object *scheme_make_raw_pair(Scheme_Object *, Scheme_Object *);

MZ_EXTERN Scheme_Object *scheme_make_byte_string(const char *chars);
MZ_EXTERN Scheme_Object *scheme_make_sized_byte_string(char *chars, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_sized_offset_byte_string(char *chars, long d, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_immutable_sized_byte_string(char *chars, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_byte_string_without_copying(char *chars);
MZ_EXTERN Scheme_Object *scheme_alloc_byte_string(int size, char fill);
MZ_EXTERN Scheme_Object *scheme_append_byte_string(Scheme_Object *, Scheme_Object *);

MZ_EXTERN Scheme_Object *scheme_make_utf8_string(const char *chars);
MZ_EXTERN Scheme_Object *scheme_make_sized_utf8_string(char *chars, long len);
MZ_EXTERN Scheme_Object *scheme_make_sized_offset_utf8_string(char *chars, long d, long len);
MZ_EXTERN Scheme_Object *scheme_make_immutable_sized_utf8_string(char *chars, long len);

MZ_EXTERN Scheme_Object *scheme_make_locale_string(const char *chars);

MZ_EXTERN Scheme_Object *scheme_char_string_to_byte_string(Scheme_Object *s);
MZ_EXTERN Scheme_Object *scheme_byte_string_to_char_string(Scheme_Object *s);
MZ_EXTERN Scheme_Object *scheme_char_string_to_byte_string_locale(Scheme_Object *s);
MZ_EXTERN Scheme_Object *scheme_byte_string_to_char_string_locale(Scheme_Object *s);
MZ_EXTERN Scheme_Object *scheme_char_string_to_path(Scheme_Object *p);
MZ_EXTERN Scheme_Object *scheme_path_to_char_string(Scheme_Object *p);

MZ_EXTERN Scheme_Object *scheme_make_char_string(const mzchar *chars);
MZ_EXTERN Scheme_Object *scheme_make_sized_char_string(mzchar *chars, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_sized_offset_char_string(mzchar *chars, long d, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_immutable_sized_char_string(mzchar *chars, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_char_string_without_copying(mzchar *chars);
MZ_EXTERN Scheme_Object *scheme_alloc_char_string(int size, mzchar fill);
MZ_EXTERN Scheme_Object *scheme_append_char_string(Scheme_Object *, Scheme_Object *);

MZ_EXTERN mzchar *scheme_string_recase(mzchar *s, int d, int len, int mode, int inplace, int *_len);

MZ_EXTERN Scheme_Object *scheme_make_vector(int size, Scheme_Object *fill);
MZ_EXTERN Scheme_Object *scheme_make_integer_value(long i);
MZ_EXTERN Scheme_Object *scheme_make_integer_value_from_unsigned(unsigned long i);
MZ_EXTERN Scheme_Object *scheme_make_integer_value_from_long_long(mzlonglong i);
MZ_EXTERN Scheme_Object *scheme_make_integer_value_from_unsigned_long_long(umzlonglong i);
MZ_EXTERN Scheme_Object *scheme_make_integer_value_from_long_halves(unsigned long lowhalf, unsigned long hihalf);
MZ_EXTERN Scheme_Object *scheme_make_integer_value_from_unsigned_long_halves(unsigned long lowhalf, unsigned long hihalf);
MZ_EXTERN Scheme_Object *scheme_make_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
MZ_EXTERN Scheme_Object *scheme_make_float(float f) ;
#endif
MZ_EXTERN Scheme_Object *scheme_make_char(mzchar ch);
MZ_EXTERN Scheme_Object *scheme_make_char_or_nul(mzchar ch);
MZ_EXTERN Scheme_Object *scheme_make_sema(long v);
MZ_EXTERN void scheme_post_sema(Scheme_Object *o);
MZ_EXTERN void scheme_post_sema_all(Scheme_Object *o);
MZ_EXTERN int scheme_wait_sema(Scheme_Object *o, int just_try);
XFORM_NONGCING MZ_EXTERN int scheme_try_plain_sema(Scheme_Object *o);
MZ_EXTERN Scheme_Object **scheme_char_constants;

MZ_EXTERN Scheme_Object *scheme_make_channel();
MZ_EXTERN Scheme_Object *scheme_make_channel_put_evt(Scheme_Object *ch, Scheme_Object *v);

XFORM_NONGCING MZ_EXTERN int scheme_get_int_val(Scheme_Object *o, long *v);
XFORM_NONGCING MZ_EXTERN int scheme_get_unsigned_int_val(Scheme_Object *o, unsigned long *v);
XFORM_NONGCING MZ_EXTERN int scheme_get_long_long_val(Scheme_Object *o, mzlonglong *v);
XFORM_NONGCING MZ_EXTERN int scheme_get_unsigned_long_long_val(Scheme_Object *o, umzlonglong *v);

XFORM_NONGCING MZ_EXTERN double scheme_real_to_double(Scheme_Object *r);

MZ_EXTERN Scheme_Object *scheme_make_cptr(void *cptr, Scheme_Object *typetag);
MZ_EXTERN Scheme_Object *scheme_make_offset_cptr(void *cptr, long offset, Scheme_Object *typetag);

MZ_EXTERN const char *scheme_get_proc_name(Scheme_Object *p, int *len, int for_error);

/*========================================================================*/
/*                               strings                                  */
/*========================================================================*/

MZ_EXTERN int scheme_utf8_decode(const unsigned char *s, int start, int end, 
				 unsigned int *us, int dstart, int dend,
				 long *ipos, char utf16, int permissive);
MZ_EXTERN int scheme_utf8_decode_as_prefix(const unsigned char *s, int start, int end, 
					   unsigned int *us, int dstart, int dend,
					   long *ipos, char utf16, int permissive);
MZ_EXTERN int scheme_utf8_decode_all(const unsigned char *s, int len, unsigned int *us, 
				     int permissive);
MZ_EXTERN int scheme_utf8_decode_prefix(const unsigned char *s, int len, unsigned int *us, 
					int permissive);
MZ_EXTERN mzchar *scheme_utf8_decode_to_buffer(const unsigned char *s, int len, 
					       mzchar *buf, int blen);
MZ_EXTERN mzchar *scheme_utf8_decode_to_buffer_len(const unsigned char *s, int len, 
						   mzchar *buf, int blen, long *rlen);
XFORM_NONGCING MZ_EXTERN int scheme_utf8_decode_count(const unsigned char *s, int start, int end, 
						      int *_state, int might_continue, int permissive);

MZ_EXTERN int scheme_utf8_encode(const unsigned int *us, int start, int end, 
				 unsigned char *s, int dstart,
				 char utf16);
MZ_EXTERN int scheme_utf8_encode_all(const unsigned int *us, int len, unsigned char *s);

MZ_EXTERN char *scheme_utf8_encode_to_buffer(const mzchar *s, int len, 
					     char *buf, int blen);
MZ_EXTERN char *scheme_utf8_encode_to_buffer_len(const mzchar *s, int len, 
						 char *buf, int blen, long *rlen);

MZ_EXTERN unsigned short *scheme_ucs4_to_utf16(const mzchar *text, int start, int end, 
					       unsigned short *buf, int bufsize,
					       long *ulen, int term_size);
MZ_EXTERN mzchar *scheme_utf16_to_ucs4(const unsigned short *text, int start, int end, 
				       mzchar *buf, int bufsize,
				       long *ulen, int term_size);

MZ_EXTERN Scheme_Object *scheme_open_converter(const char *from_e, const char *to_e);
MZ_EXTERN void scheme_close_converter(Scheme_Object *conv);

/*========================================================================*/
/*                               bignums                                  */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_make_bignum(long v);
MZ_EXTERN Scheme_Object *scheme_make_bignum_from_unsigned(unsigned long v);
MZ_EXTERN Scheme_Object *scheme_make_bignum_from_long_long(mzlonglong v);
MZ_EXTERN Scheme_Object *scheme_make_bignum_from_unsigned_long_long(umzlonglong v);
MZ_EXTERN double scheme_bignum_to_double(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_bignum_from_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
MZ_EXTERN float scheme_bignum_to_float(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_bignum_from_float(float d);
#else
# define scheme_bignum_to_float scheme_bignum_to_double
# define scheme_bignum_from_float scheme_bignum_from_double
#endif
MZ_EXTERN char *scheme_bignum_to_string(const Scheme_Object *n, int radix);
MZ_EXTERN char *scheme_bignum_to_allocated_string(const Scheme_Object *n, int radix, int alloc);
MZ_EXTERN Scheme_Object *scheme_read_bignum(const mzchar *str, int offset, int radix);
MZ_EXTERN Scheme_Object *scheme_read_bignum_bytes(const char *str, int offset, int radix);
XFORM_NONGCING MZ_EXTERN Scheme_Object *scheme_bignum_normalize(const Scheme_Object *n);

/*========================================================================*/
/*                              rationals                                 */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_make_rational(const Scheme_Object *r, const Scheme_Object *d);
XFORM_NONGCING MZ_EXTERN double scheme_rational_to_double(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_rational_from_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
XFORM_NONGCING MZ_EXTERN float scheme_rational_to_float(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_rational_from_float(float d);
#else
# define scheme_rational_to_float scheme_rational_to_double
# define scheme_rational_from_float scheme_rational_from_double
#endif
MZ_EXTERN Scheme_Object *scheme_rational_normalize(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_rational_numerator(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_rational_denominator(const Scheme_Object *n);

/*========================================================================*/
/*                              complexes                                 */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_make_complex(const Scheme_Object *r, const Scheme_Object *i);
MZ_EXTERN Scheme_Object *scheme_complex_normalize(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_complex_real_part(const Scheme_Object *n);
MZ_EXTERN Scheme_Object *scheme_complex_imaginary_part(const Scheme_Object *n);

/* Exact/inexact: */
XFORM_NONGCING MZ_EXTERN int scheme_is_exact(const Scheme_Object *n);
XFORM_NONGCING MZ_EXTERN int scheme_is_inexact(const Scheme_Object *n);

/*========================================================================*/
/*                 macros, syntax, and compilation                        */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_expand(Scheme_Object *form, Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable);

/*========================================================================*/
/*                               ports                                    */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_read(Scheme_Object *port);
MZ_EXTERN Scheme_Object *scheme_read_syntax(Scheme_Object *port, Scheme_Object *stxsrc);
MZ_EXTERN void scheme_write(Scheme_Object *obj, Scheme_Object *port);
MZ_EXTERN void scheme_display(Scheme_Object *obj, Scheme_Object *port);
MZ_EXTERN void scheme_print(Scheme_Object *obj, Scheme_Object *port);
MZ_EXTERN void scheme_write_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl);
MZ_EXTERN void scheme_display_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl);
MZ_EXTERN void scheme_print_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl);
MZ_EXTERN void scheme_write_byte_string(const char *str, long len, Scheme_Object *port);
MZ_EXTERN void scheme_write_char_string(const mzchar *str, long len, Scheme_Object *port);
MZ_EXTERN long scheme_put_byte_string(const char *who, Scheme_Object *port,
				      const char *str, long d, long len,
				      int rarely_block);
MZ_EXTERN long scheme_put_char_string(const char *who, Scheme_Object *port,
				      const mzchar *str, long d, long len);
MZ_EXTERN char *scheme_write_to_string(Scheme_Object *obj, long *len);
MZ_EXTERN char *scheme_display_to_string(Scheme_Object *obj, long *len);
MZ_EXTERN char *scheme_print_to_string(Scheme_Object *obj, long *len);
MZ_EXTERN char *scheme_write_to_string_w_max(Scheme_Object *obj, long *len, long maxl);
MZ_EXTERN char *scheme_display_to_string_w_max(Scheme_Object *obj, long *len, long maxl);
MZ_EXTERN char *scheme_print_to_string_w_max(Scheme_Object *obj, long *len, long maxl);
MZ_EXTERN void scheme_debug_print(Scheme_Object *obj);
MZ_EXTERN void scheme_flush_output(Scheme_Object *port);

MZ_EXTERN char *scheme_format(mzchar *format, int flen, int argc, Scheme_Object **argv, long *rlen);
MZ_EXTERN void scheme_printf(mzchar *format, int flen, int argc, Scheme_Object **argv);
MZ_EXTERN char *scheme_format_utf8(char *format, int flen, int argc, Scheme_Object **argv, long *rlen);
MZ_EXTERN void scheme_printf_utf8(char *format, int flen, int argc, Scheme_Object **argv);

MZ_EXTERN int scheme_getc(Scheme_Object *port);
MZ_EXTERN int scheme_get_byte(Scheme_Object *port);
MZ_EXTERN int scheme_peekc(Scheme_Object *port);
MZ_EXTERN int scheme_peek_byte(Scheme_Object *port);
MZ_EXTERN int scheme_peekc_skip(Scheme_Object *port, Scheme_Object *skip);
MZ_EXTERN int scheme_peek_byte_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt);
MZ_EXTERN int scheme_getc_special_ok(Scheme_Object *port);
MZ_EXTERN int scheme_get_byte_special_ok(Scheme_Object *port);
MZ_EXTERN int scheme_peekc_special_ok(Scheme_Object *port);
MZ_EXTERN int scheme_peek_byte_special_ok_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt);
MZ_EXTERN int scheme_peekc_special_ok_skip(Scheme_Object *port, Scheme_Object *skip);
MZ_EXTERN void scheme_ungetc(int ch, Scheme_Object *port);
MZ_EXTERN int scheme_byte_ready(Scheme_Object *port);
MZ_EXTERN int scheme_char_ready(Scheme_Object *port);
MZ_EXTERN int scheme_peekc_is_ungetc(Scheme_Object *port);
MZ_EXTERN void scheme_need_wakeup(Scheme_Object *port, void *fds);
MZ_EXTERN long scheme_get_byte_string(const char *who,
				      Scheme_Object *port,
				      char *buffer, long offset, long size,
				      int only_avail,
				      int peek, Scheme_Object *peek_skip);
MZ_EXTERN long scheme_get_byte_string_unless(const char *who,
					     Scheme_Object *port,
					     char *buffer, long offset, long size,
					     int only_avail,
					     int peek, Scheme_Object *peek_skip,
					     Scheme_Object *unless_evt);
MZ_EXTERN long scheme_get_byte_string_special_ok_unless(const char *who,
							Scheme_Object *port,
							char *buffer, long offset, long size,
							int only_avail,
							int peek, Scheme_Object *peek_skip,
							Scheme_Object *unless_evt);
MZ_EXTERN Scheme_Object *scheme_progress_evt(Scheme_Object *port);
MZ_EXTERN int scheme_peeked_read(Scheme_Object *port,
				 long size,
				 Scheme_Object *unless_evt,
				 Scheme_Object *target_evt);
MZ_EXTERN long scheme_get_char_string(const char *who,
				      Scheme_Object *port,
				      mzchar *buffer, long offset, long size,
				      int peek, Scheme_Object *peek_skip);
MZ_EXTERN long scheme_get_bytes(Scheme_Object *port, long size, char *buffer, int offset);
MZ_EXTERN Scheme_Object *scheme_get_ready_special(Scheme_Object *port, Scheme_Object *stxsrc, int peek);
MZ_EXTERN long scheme_tell(Scheme_Object *port);
MZ_EXTERN long scheme_output_tell(Scheme_Object *port);
MZ_EXTERN long scheme_tell_line(Scheme_Object *port);
MZ_EXTERN long scheme_tell_column(Scheme_Object *port);
MZ_EXTERN void scheme_tell_all(Scheme_Object *port, long *line, long *col, long *pos);
MZ_EXTERN void scheme_count_lines(Scheme_Object *port);
MZ_EXTERN void scheme_close_input_port(Scheme_Object *port);
MZ_EXTERN void scheme_close_output_port(Scheme_Object *port);

MZ_EXTERN Scheme_Object *scheme_write_special(int argc, Scheme_Object *argv[]);
MZ_EXTERN Scheme_Object *scheme_write_special_nonblock(int argc, Scheme_Object *argv[]);

MZ_EXTERN Scheme_Object *scheme_make_write_evt(const char *who, Scheme_Object *port,
					       Scheme_Object *special, char *str, long start, long size);

MZ_EXTERN Scheme_Port *scheme_port_record(Scheme_Object *port);
MZ_EXTERN Scheme_Input_Port *scheme_input_port_record(Scheme_Object *port);
MZ_EXTERN Scheme_Output_Port *scheme_output_port_record(Scheme_Object *port);
XFORM_NONGCING MZ_EXTERN int scheme_is_input_port(Scheme_Object *port);
XFORM_NONGCING MZ_EXTERN int scheme_is_output_port(Scheme_Object *port);

MZ_EXTERN Scheme_Object *scheme_make_port_type(const char *name);
MZ_EXTERN Scheme_Input_Port *scheme_make_input_port(Scheme_Object *subtype, void *data,
						    Scheme_Object *name,
						    Scheme_Get_String_Fun get_byte_string_fun,
						    Scheme_Peek_String_Fun peek_string_fun,
						    Scheme_Progress_Evt_Fun progress_evt_fun,
						    Scheme_Peeked_Read_Fun peeked_read_fun,
						    Scheme_In_Ready_Fun byte_ready_fun,
						    Scheme_Close_Input_Fun close_fun,
						    Scheme_Need_Wakeup_Input_Fun need_wakeup_fun,
						    int must_close);
MZ_EXTERN Scheme_Output_Port *scheme_make_output_port(Scheme_Object *subtype, void *data,
						      Scheme_Object *name,
						      Scheme_Write_String_Evt_Fun write_byte_string_evt_fun,
						      Scheme_Write_String_Fun write_byte_string_fun,
						      Scheme_Out_Ready_Fun ready_fun,
						      Scheme_Close_Output_Fun close_fun,
						      Scheme_Need_Wakeup_Output_Fun need_wakeup_fun,
						      Scheme_Write_Special_Evt_Fun write_special_evt_fun,
						      Scheme_Write_Special_Fun write_special_fun,
						      int must_close);

MZ_EXTERN void scheme_set_port_location_fun(Scheme_Port *port,
					    Scheme_Location_Fun location_fun);
MZ_EXTERN void scheme_set_port_count_lines_fun(Scheme_Port *port,
					       Scheme_Count_Lines_Fun count_lines_fun);

MZ_EXTERN Scheme_Object *scheme_progress_evt_via_get(Scheme_Input_Port *port);
MZ_EXTERN int scheme_peeked_read_via_get(Scheme_Input_Port *port,
					 long size,
					 Scheme_Object *unless_evt,
					 Scheme_Object *target_ch);
MZ_EXTERN Scheme_Object *scheme_write_evt_via_write(Scheme_Output_Port *port,
						    const char *str, long offset, long size);
MZ_EXTERN Scheme_Object *scheme_write_special_evt_via_write_special(Scheme_Output_Port *port, 
								    Scheme_Object *special);

MZ_EXTERN Scheme_Object *scheme_open_input_file(const char *name, const char *who);
MZ_EXTERN Scheme_Object *scheme_open_output_file(const char *name, const char *who);
MZ_EXTERN Scheme_Object *scheme_open_output_file_with_mode(const char *name, const char *who, int text);

MZ_EXTERN Scheme_Object *scheme_make_file_input_port(FILE *fp);
MZ_EXTERN Scheme_Object *scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name);
MZ_EXTERN Scheme_Object *scheme_make_file_output_port(FILE *fp);

MZ_EXTERN Scheme_Object *scheme_make_fd_input_port(int fd, Scheme_Object *name, int regfile, int win_textmode);
MZ_EXTERN Scheme_Object *scheme_make_fd_output_port(int fd, Scheme_Object *name, int regfile, int win_textmode, int read_too);

MZ_EXTERN Scheme_Object *scheme_make_byte_string_input_port(const char *str);
MZ_EXTERN Scheme_Object *scheme_make_sized_byte_string_input_port(const char *str, long len);
MZ_EXTERN Scheme_Object *scheme_make_byte_string_output_port();
MZ_EXTERN char *scheme_get_sized_byte_string_output(Scheme_Object *port, long *len);
MZ_EXTERN char *scheme_get_reset_sized_byte_string_output(Scheme_Object *port, long *len, int reset, long startpos, long endpos);

MZ_EXTERN void scheme_pipe(Scheme_Object **read, Scheme_Object **write);
MZ_EXTERN void scheme_pipe_with_limit(Scheme_Object **write, Scheme_Object **read, int maxsize);

MZ_EXTERN Scheme_Object *scheme_make_null_output_port(int can_write_special);
MZ_EXTERN Scheme_Object *scheme_make_redirect_output_port(Scheme_Object *port);

MZ_EXTERN long scheme_set_file_position(Scheme_Object *port, long pos);

MZ_EXTERN int scheme_file_exists(char *filename);
MZ_EXTERN int scheme_directory_exists(char *dirname);
MZ_EXTERN char *scheme_expand_filename(char* filename, int ilen, const char *errorin, int *ex, int guards);
MZ_EXTERN char *scheme_expand_string_filename(Scheme_Object *f, const char *errorin, int *ex, int guards);

MZ_EXTERN char *scheme_os_getcwd(char *buf, int buflen, int *actlen, int noexn);
MZ_EXTERN int scheme_os_setcwd(char *buf, int noexn);
MZ_EXTERN char *scheme_getdrive(void);

MZ_EXTERN Scheme_Object *scheme_split_path(const char *path, int len, Scheme_Object **base, int *isdir, int kind);
MZ_EXTERN Scheme_Object *scheme_build_path(int argc, Scheme_Object **argv);
MZ_EXTERN Scheme_Object *scheme_path_to_directory_path(Scheme_Object *p);
MZ_EXTERN Scheme_Object *scheme_path_to_complete_path(Scheme_Object *path, Scheme_Object *relto_path);

MZ_EXTERN Scheme_Object *scheme_make_path(const char *chars);
MZ_EXTERN Scheme_Object *scheme_make_sized_path(char *chars, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_sized_offset_path(char *chars, long d, long len, int copy);
MZ_EXTERN Scheme_Object *scheme_make_sized_offset_kind_path(char *chars, long d, long len, int copy, int kind);
MZ_EXTERN Scheme_Object *scheme_make_path_without_copying(char *chars);

#ifdef MACINTOSH_EVENTS
MZ_EXTERN char *scheme_mac_spec_to_path(mzFSSpec *spec);
MZ_EXTERN int scheme_mac_path_to_spec(const char *filename, mzFSSpec *spec);
#endif

MZ_EXTERN void *scheme_alloc_fdset_array(int count, int permanent);
MZ_EXTERN void *scheme_init_fdset_array(void *fdarray, int count);
MZ_EXTERN void *scheme_get_fdset(void *fdarray, int pos);
MZ_EXTERN void scheme_fdzero(void *fd);
MZ_EXTERN void scheme_fdset(void *fd, int pos);
MZ_EXTERN void scheme_fdclr(void *fd, int pos);
MZ_EXTERN int scheme_fdisset(void *fd, int pos);
MZ_EXTERN void scheme_add_fd_handle(void *h, void *fds, int repost);
MZ_EXTERN void scheme_add_fd_eventmask(void *fds, int mask);

MZ_EXTERN void scheme_security_check_file(const char *who, const char *filename, int guards);
MZ_EXTERN void scheme_security_check_file_link(const char *who, const char *filename, const char *content);
MZ_EXTERN void scheme_security_check_network(const char *who, const char *host, int port, int client);

MZ_EXTERN struct mz_addrinfo *scheme_get_host_address(const char *address, int id, int *err, 
						      int family, int passive, int tcp);
MZ_EXTERN void scheme_free_host_address(struct mz_addrinfo *a);
MZ_EXTERN const char *scheme_host_address_strerror(int errnum);
MZ_EXTERN void scheme_getnameinfo(void *sa, int salen, 
				  char *host, int hostlen,
				  char *serv, int servlen);

MZ_EXTERN int scheme_get_port_file_descriptor(Scheme_Object *p, long *_fd);
MZ_EXTERN long scheme_get_port_fd(Scheme_Object *p);
MZ_EXTERN int scheme_get_port_socket(Scheme_Object *p, long *_s);
MZ_EXTERN void scheme_socket_to_ports(long s, const char *name, int takeover,
                                      Scheme_Object **_inp, Scheme_Object **_outp);

MZ_EXTERN void scheme_set_type_printer(Scheme_Type stype, Scheme_Type_Printer printer);
MZ_EXTERN void scheme_print_bytes(Scheme_Print_Params *pp, const char *str, int offset, int len);
MZ_EXTERN void scheme_print_utf8(Scheme_Print_Params *pp, const char *str, int offset, int len);
MZ_EXTERN void scheme_print_string(Scheme_Print_Params *pp, const mzchar *str, int offset, int len);

MZ_EXTERN Scheme_Object *scheme_read_byte_string(Scheme_Object *port);

/*========================================================================*/
/*                        namespace/environment                           */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[]);
MZ_EXTERN void scheme_add_namespace_option(Scheme_Object *key, void (*f)(Scheme_Env *));

MZ_EXTERN void scheme_require_from_original_env(Scheme_Env *env, int syntax_only);

MZ_EXTERN void scheme_add_global(const char *name, Scheme_Object *val, Scheme_Env *env);
MZ_EXTERN void scheme_add_global_symbol(Scheme_Object *name, Scheme_Object *val,
			      Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_make_envunbox(Scheme_Object *value);

MZ_EXTERN Scheme_Object *scheme_lookup_global(Scheme_Object *symbol, Scheme_Env *env);

MZ_EXTERN Scheme_Bucket *scheme_global_bucket(Scheme_Object *symbol, Scheme_Env *env);
MZ_EXTERN Scheme_Bucket *scheme_global_keyword_bucket(Scheme_Object *symbol, Scheme_Env *env);
MZ_EXTERN Scheme_Bucket *scheme_module_bucket(Scheme_Object *mod, Scheme_Object *var, int pos, Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_builtin_value(const char *name); /* convenience */

MZ_EXTERN void scheme_set_global_bucket(char *proc, Scheme_Bucket *var, Scheme_Object *val,
			      int set_undef);
MZ_EXTERN void scheme_install_macro(Scheme_Bucket *b, Scheme_Object *v);


MZ_EXTERN void scheme_save_initial_module_set(Scheme_Env *env);
MZ_EXTERN Scheme_Env *scheme_primitive_module(Scheme_Object *name, Scheme_Env *for_env);
MZ_EXTERN void scheme_finish_primitive_module(Scheme_Env *env);
MZ_EXTERN void scheme_protect_primitive_provide(Scheme_Env *env, Scheme_Object *name);

MZ_EXTERN Scheme_Object *scheme_make_modidx(Scheme_Object *path,
				  Scheme_Object *base,
				  Scheme_Object *resolved);

MZ_EXTERN Scheme_Object *scheme_apply_for_syntax_in_env(Scheme_Object *proc, Scheme_Env *env);

MZ_EXTERN Scheme_Object *scheme_dynamic_require(int argc, Scheme_Object *argv[]);

/*========================================================================*/
/*                                symbols                                 */
/*========================================================================*/

MZ_EXTERN Scheme_Object *scheme_intern_symbol(const char *name);
MZ_EXTERN Scheme_Object *scheme_intern_exact_symbol(const char *name, unsigned int len);
MZ_EXTERN Scheme_Object *scheme_intern_exact_char_symbol(const mzchar *name, unsigned int len);
MZ_EXTERN Scheme_Object *scheme_make_symbol(const char *name); /* Make uninterned */
MZ_EXTERN Scheme_Object *scheme_make_exact_symbol(const char *name, unsigned int len); /* Exact case */
MZ_EXTERN Scheme_Object *scheme_make_exact_char_symbol(const mzchar *name, unsigned int len); /* Exact case */
MZ_EXTERN const char *scheme_symbol_name(Scheme_Object *sym);
MZ_EXTERN const char *scheme_symbol_name_and_size(Scheme_Object *sym, unsigned int *l, int flags);
MZ_EXTERN char *scheme_symbol_val(Scheme_Object *sym);

MZ_EXTERN Scheme_Object *scheme_intern_exact_keyword(const char *name, unsigned int len);
MZ_EXTERN Scheme_Object *scheme_intern_exact_char_keyword(const mzchar *name, unsigned int len);

/*========================================================================*/
/*                                structs                                 */
/*========================================================================*/

MZ_EXTERN Scheme_Object **scheme_make_struct_values(Scheme_Object *struct_type,
					  Scheme_Object **names,
					  int count, int flags);
MZ_EXTERN Scheme_Object **scheme_make_struct_names(Scheme_Object *base,
					 Scheme_Object *field_names,
					 int flags, int *count_out);
MZ_EXTERN Scheme_Object *scheme_make_struct_type(Scheme_Object *base,
						 Scheme_Object *parent,
						 Scheme_Object *inspector,
						 int num_fields, int num_uninit_fields,
						 Scheme_Object *uninit_val,
						 Scheme_Object *properties,
						 Scheme_Object *guard);
MZ_EXTERN Scheme_Object *scheme_make_struct_instance(Scheme_Object *stype,
						     int argc,
						     Scheme_Object **argv);

MZ_EXTERN Scheme_Object *scheme_make_struct_exptime(Scheme_Object **names, int count,
						    Scheme_Object *super_sym,
						    Scheme_Object *super_exptime,
						    int flags);

XFORM_NONGCING MZ_EXTERN int scheme_is_struct_instance(Scheme_Object *type, Scheme_Object *v);
MZ_EXTERN Scheme_Object *scheme_struct_ref(Scheme_Object *s, int pos);
MZ_EXTERN void scheme_struct_set(Scheme_Object *s, int pos, Scheme_Object *v);

MZ_EXTERN Scheme_Object *scheme_make_struct_type_property(Scheme_Object *name);
MZ_EXTERN Scheme_Object *scheme_make_struct_type_property_w_guard(Scheme_Object *name, Scheme_Object *guard);
XFORM_NONGCING MZ_EXTERN Scheme_Object *scheme_struct_type_property_ref(Scheme_Object *prop, Scheme_Object *s);

MZ_EXTERN Scheme_Object *scheme_make_location(Scheme_Object *src,
					      Scheme_Object *line,
					      Scheme_Object *col,
					      Scheme_Object *pos,
					      Scheme_Object *span);
MZ_EXTERN int scheme_is_location(Scheme_Object *o);

MZ_EXTERN Scheme_Object *scheme_make_inspector(Scheme_Object *superior);
XFORM_NONGCING MZ_EXTERN int scheme_is_subinspector(Scheme_Object *i, Scheme_Object *sup);

/*========================================================================*/
/*                              utilities                                 */
/*========================================================================*/

XFORM_NONGCING MZ_EXTERN int scheme_eq(Scheme_Object *obj1, Scheme_Object *obj2);
XFORM_NONGCING MZ_EXTERN int scheme_eqv(Scheme_Object *obj1, Scheme_Object *obj2);
MZ_EXTERN int scheme_equal(Scheme_Object *obj1, Scheme_Object *obj2);

#ifdef MZ_PRECISE_GC
XFORM_NONGCING MZ_EXTERN long scheme_hash_key(Scheme_Object *o);
#endif
MZ_EXTERN long scheme_equal_hash_key(Scheme_Object *o);
MZ_EXTERN long scheme_equal_hash_key2(Scheme_Object *o);

MZ_EXTERN void scheme_set_type_equality(Scheme_Type type, 
                                        Scheme_Equal_Proc f,
                                        Scheme_Primary_Hash_Proc hash1,
                                        Scheme_Secondary_Hash_Proc hash2);

MZ_EXTERN Scheme_Object *scheme_build_list(int argc, Scheme_Object **argv);
MZ_EXTERN Scheme_Object *scheme_build_list_offset(int argc, Scheme_Object **argv, int delta);
MZ_EXTERN void scheme_make_list_immutable(Scheme_Object *l);

MZ_EXTERN int scheme_list_length(Scheme_Object *list);
MZ_EXTERN int scheme_proper_list_length(Scheme_Object *list);

MZ_EXTERN Scheme_Object *scheme_alloc_list(int size);
MZ_EXTERN Scheme_Object *scheme_map_1(Scheme_Object *(*f)(Scheme_Object*),
			    Scheme_Object *l);

MZ_EXTERN Scheme_Object *scheme_car(Scheme_Object *pair);
MZ_EXTERN Scheme_Object *scheme_cdr(Scheme_Object *pair);
MZ_EXTERN Scheme_Object *scheme_cadr(Scheme_Object *pair);
MZ_EXTERN Scheme_Object *scheme_caddr(Scheme_Object *pair);

MZ_EXTERN Scheme_Object *scheme_vector_to_list(Scheme_Object *vec);
MZ_EXTERN Scheme_Object *scheme_list_to_vector(Scheme_Object *list);

MZ_EXTERN Scheme_Object *scheme_append(Scheme_Object *lstx, Scheme_Object *lsty);
MZ_EXTERN Scheme_Object *scheme_reverse(Scheme_Object *l);

MZ_EXTERN Scheme_Object *scheme_box(Scheme_Object *v);
MZ_EXTERN Scheme_Object *scheme_unbox(Scheme_Object *obj);
MZ_EXTERN void scheme_set_box(Scheme_Object *b, Scheme_Object *v);

MZ_EXTERN Scheme_Object *scheme_make_weak_box(Scheme_Object *v);

MZ_EXTERN Scheme_Object *scheme_make_ephemeron(Scheme_Object *key, Scheme_Object *val);
MZ_EXTERN Scheme_Object *scheme_ephemeron_value(Scheme_Object *o);

MZ_EXTERN Scheme_Object *scheme_load(const char *file);
MZ_EXTERN Scheme_Object *scheme_load_extension(const char *filename, Scheme_Env *env);
MZ_EXTERN void scheme_register_extension_global(void *ptr, long size);

MZ_EXTERN long scheme_get_seconds(void);
MZ_EXTERN long scheme_get_milliseconds(void);
MZ_EXTERN double scheme_get_inexact_milliseconds(void);
MZ_EXTERN long scheme_get_process_milliseconds(void);

MZ_EXTERN char *scheme_banner(void);
MZ_EXTERN char *scheme_version(void);

MZ_EXTERN int scheme_check_proc_arity(const char *where, int a,
				      int which, int argc, Scheme_Object **argv);
MZ_EXTERN int scheme_check_proc_arity2(const char *where, int a,
				       int which, int argc, Scheme_Object **argv,
				       int false_ok);

MZ_EXTERN char *scheme_make_provided_string(Scheme_Object *o, int count, int *len);
MZ_EXTERN char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv, long *len);

MZ_EXTERN void scheme_no_dumps(char *why);

MZ_EXTERN const char *scheme_system_library_subpath();

MZ_EXTERN void scheme_signal_received(void);

MZ_EXTERN int scheme_char_strlen(const mzchar *s);
