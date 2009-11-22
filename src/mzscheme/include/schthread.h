
/* Implements thread-local variables, which are all combined into one
   big table.
   
   When thread-local variables are not needed, this file doesn't
   declare them. Instead, declarations marked with
   THREAD_LOCAL_DECL() are used.

   When thread-local variables are needed, then THREAD_LOCAL_DECL()
   expands to nothing, and this file defines each thread-local variable
   as a field in one big record. The record is accessed through a single
   thread-local variable or through pthread_getspecific().

   Choose the names of thread-local variables carefully. The names are
   globally visible, and macros to redirect uses of thread-local
   variables can create trouble and poor error messages from the C
   compiler if the same name is used for a local variable. */

#ifndef SCHEME_THREADLOCAL_H
#define SCHEME_THREADLOCAL_H

#if defined(MZ_USE_PLACES) || defined(FUTURES_ENABLED)
#  define USE_THREAD_LOCAL
# if _MSC_VER
#  define THREAD_LOCAL __declspec(thread)
# elif defined(OS_X)
#  define IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS
#  if defined(__x86_64__) || defined(__i386__)
#   define INLINE_GETSPECIFIC_ASSEMBLY_CODE
#  endif
# else
#  define THREAD_LOCAL __thread
# endif
#else
# define THREAD_LOCAL /* empty */
#endif

extern void scheme_init_os_thread();

/* **************************************************************** */
/* Declarations that we wish were elsewhere, but are needed here to */
/* determine the size and type of some thread-local record fields.  */
/* **************************************************************** */

#define STACK_COPY_CACHE_SIZE 10
#define BIGNUM_CACHE_SIZE 16
#define STACK_CACHE_SIZE 32

/* This structure must be 4 words: */
typedef struct {
  void *orig_return_address;
  void *stack_frame;
  struct Scheme_Object *cache;
  void *orig_result;
} Stack_Cache_Elem;

typedef long rxpos;

struct gmp_tmp_stack
{
  void *end;
  void *alloc_point;
  struct gmp_tmp_stack *prev;
};

#ifndef MZ_PRECISE_GC
typedef long objhead;
#endif

/* **************************************** */

#ifndef USE_THREAD_LOCAL

# define THREAD_LOCAL_DECL(x) x

#else

/* **************************************** */

#if FUTURES_ENABLED
# include <pthread.h>
#endif

typedef struct Thread_Local_Variables {
  void **GC_variable_stack_;
  objhead GC_objhead_template_;
  struct NewGC *GC_;
  unsigned long GC_gen0_alloc_page_ptr_;
  unsigned long GC_gen0_alloc_page_end_;
  void *bignum_cache_[BIGNUM_CACHE_SIZE];
  int cache_count_;
  struct Scheme_Hash_Table *toplevels_ht_;
  struct Scheme_Hash_Table *locals_ht_[2];
  volatile int scheme_fuel_counter_;
  unsigned long scheme_stack_boundary_;
  unsigned long volatile scheme_jit_stack_boundary_;
  volatile int scheme_future_need_gc_pause_;
  int scheme_use_rtcall_;
  struct Scheme_Object *quick_stx_;
  int scheme_continuation_application_count_;
  int scheme_cont_capture_count_;
  int scheme_prompt_capture_count_;
  struct Scheme_Prompt *available_prompt_;
  struct Scheme_Prompt *available_cws_prompt_;
  struct Scheme_Prompt *available_regular_prompt_;
  struct Scheme_Dynamic_Wind *available_prompt_dw_;
  struct Scheme_Meta_Continuation *available_prompt_mc_;
  struct Scheme_Object *cached_beg_stx_;
  struct Scheme_Object *cached_dv_stx_;
  struct Scheme_Object *cached_ds_stx_;
  int cached_stx_phase_;
  struct Scheme_Cont *offstack_cont_;
  struct Scheme_Overflow *offstack_overflow_;
  struct Scheme_Overflow_Jmp *scheme_overflow_jmp_;
  void *scheme_overflow_stack_start_;
  void **codetab_tree_;
  int during_set_;
  Stack_Cache_Elem stack_cache_stack_[STACK_CACHE_SIZE];
  long stack_cache_stack_pos_;
  struct Scheme_Object **fixup_runstack_base_;
  int fixup_already_in_place_;
  void *retry_alloc_r1_;
  double save_fp_;
  struct Scheme_Bucket_Table *starts_table_;
  struct Scheme_Modidx *modidx_caching_chain_;
  struct Scheme_Object *global_shift_cache_;
  struct mz_proc_thread *proc_thread_self_;
  struct Scheme_Object *scheme_orig_stdout_port_;
  struct Scheme_Object *scheme_orig_stderr_port_;
  struct Scheme_Object *scheme_orig_stdin_port_;
  struct fd_set *scheme_fd_set_;
  struct Scheme_Custodian *new_port_cust_;
  int external_event_fd_;
  int put_external_event_fd_;
  char *read_string_byte_buffer_;
  struct ITimer_Data *itimerdata_;
  char *quick_buffer_;
  char *quick_encode_buffer_;
  struct Scheme_Hash_Table *cache_ht_;
  char *regstr_;
  char *regparsestr_;
  int regmatchmin_;
  int regmatchmax_;
  int regmaxbackposn_;
  int regsavepos_;
  struct Scheme_Hash_Table *regbackknown_;
  struct Scheme_Hash_Table *regbackdepends_;
  rxpos regparse_;
  rxpos regparse_end_;
  int regnpar_;
  int regncounter_;
  rxpos regcode_;
  rxpos regcodesize_;
  rxpos regcodemax_;
  long regmaxlookback_;
  long rx_buffer_size_;
  rxpos *startp_buffer_cache_;
  rxpos *endp_buffer_cache_;
  rxpos *maybep_buffer_cache_;
  unsigned long scheme_os_thread_stack_base_;
  int traversers_registered_;
  struct Finalizations **save_fns_ptr_;
  struct Scheme_Object *scheme_system_idle_channel_;
  struct Scheme_Object *system_idle_put_evt_;
  void *stack_copy_cache_[STACK_COPY_CACHE_SIZE];
  long stack_copy_size_cache_[STACK_COPY_CACHE_SIZE];
  int scc_pos_;
  struct Scheme_Bucket_Table *prefab_table_;
  struct Scheme_Object *nominal_ipair_cache_;
  struct Scheme_Object *mark_id_;
  struct Scheme_Object *current_rib_timestamp_;
  struct Scheme_Hash_Table *quick_hash_table_;
  struct Scheme_Object *last_phase_shift_;
  struct Scheme_Object *unsealed_dependencies_;
  struct Scheme_Hash_Table *id_marks_ht_;
  struct Scheme_Hash_Table *than_id_marks_ht_;
  struct Scheme_Bucket_Table *interned_skip_ribs_;
  struct Scheme_Thread *scheme_current_thread_;
  struct Scheme_Thread *scheme_main_thread_;
  struct Scheme_Thread *scheme_first_thread_;
  struct Scheme_Thread_Set *scheme_thread_set_top_;
  int num_running_threads_;
  int swap_no_setjmp_;
  int thread_swap_count_;
  int scheme_did_gc_count_;
  struct Scheme_Future_State *scheme_future_state_;
  struct Scheme_Future_Thread_State *scheme_future_thread_state_;
  void *jit_future_storage_[2];
  struct Scheme_Object **scheme_current_runstack_start_;
  struct Scheme_Object **scheme_current_runstack_;
  MZ_MARK_STACK_TYPE scheme_current_cont_mark_stack_;
  MZ_MARK_POS_TYPE scheme_current_cont_mark_pos_;
  struct Scheme_Custodian *main_custodian_;
  struct Scheme_Custodian *last_custodian_;
  struct Scheme_Hash_Table *limited_custodians_;
  struct Scheme_Thread *swap_target_;
  struct Scheme_Object *scheduled_kills_;
  int do_atomic_;
  int missed_context_switch_;
  int have_activity_;
  int scheme_active_but_sleeping_;
  int thread_ended_with_activity_;
  int scheme_no_stack_overflow_;
  int needs_sleep_cancelled_;
  int tls_pos_;
  struct Scheme_Object *the_nested_exn_handler_;
  struct Scheme_Object *cust_closers_;
  struct Scheme_Object *thread_swap_callbacks_;
  struct Scheme_Object *thread_swap_out_callbacks_;
  struct Scheme_Object *recycle_cell_;
  struct Scheme_Object *maybe_recycle_cell_;
  int recycle_cc_count_;
  mz_jmp_buf main_init_error_buf_;
  void *gmp_mem_pool_;
  unsigned long max_total_allocation_;
  unsigned long current_total_allocation_;
  struct gmp_tmp_stack gmp_tmp_xxx_;
  struct gmp_tmp_stack *gmp_tmp_current_;
} Thread_Local_Variables;

#if defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS)
/* Using Pthread getspecific() */
# include <pthread.h>
MZ_EXTERN pthread_key_t scheme_thread_local_key;
# ifndef INLINE_GETSPECIFIC_ASSEMBLY_CODE
#  define scheme_get_thread_local_variables() ((Thread_Local_Variables *)pthread_getspecific(scheme_thread_local_key))
#  ifdef MZ_XFORM
XFORM_GC_VARIABLE_STACK_THROUGH_GETSPECIFIC;
#  endif
# else
#  ifdef MZ_XFORM
START_XFORM_SKIP;
#  endif
static inline Thread_Local_Variables *scheme_get_thread_local_variables() __attribute__((used));
static inline Thread_Local_Variables *scheme_get_thread_local_variables() {
  Thread_Local_Variables *x;
#  if defined(__x86_64__)
  asm volatile("movq %%gs:0x8A0, %0" : "=r"(x));
#  else
  asm volatile("movl %%gs:0x468, %0" : "=r"(x));
#  endif
  return x;
}
#  ifdef MZ_XFORM
END_XFORM_SKIP;
XFORM_GC_VARIABLE_STACK_THROUGH_FUNCTION;
#  endif
# endif
#else
/* Using `THREAD_LOCAL' variable: */
MZ_EXTERN THREAD_LOCAL Thread_Local_Variables scheme_thread_locals;
# define scheme_get_thread_local_variables() (&scheme_thread_locals)
#ifdef MZ_XFORM
XFORM_GC_VARIABLE_STACK_THROUGH_THREAD_LOCAL;
# endif
#endif

/* **************************************** */

#ifdef MZ_XFORM
# define XOA XFORM_OK_ASSIGN
#else
# define XOA /* empty */
#endif

#define GC_objhead_template XOA (scheme_get_thread_local_variables()->GC_objhead_template_)
#define GC XOA (scheme_get_thread_local_variables()->GC_)
#define GC_gen0_alloc_page_ptr XOA (scheme_get_thread_local_variables()->GC_gen0_alloc_page_ptr_)
#define GC_gen0_alloc_page_end XOA (scheme_get_thread_local_variables()->GC_gen0_alloc_page_end_)
#define GC_variable_stack XOA (scheme_get_thread_local_variables()->GC_variable_stack_)
#define bignum_cache XOA (scheme_get_thread_local_variables()->bignum_cache_)
#define cache_count XOA (scheme_get_thread_local_variables()->cache_count_)
#define toplevels_ht XOA (scheme_get_thread_local_variables()->toplevels_ht_)
#define locals_ht XOA (scheme_get_thread_local_variables()->locals_ht_)
#define scheme_fuel_counter XOA (scheme_get_thread_local_variables()->scheme_fuel_counter_)
#define scheme_stack_boundary XOA (scheme_get_thread_local_variables()->scheme_stack_boundary_)
#define scheme_jit_stack_boundary XOA (scheme_get_thread_local_variables()->scheme_jit_stack_boundary_)
#define scheme_future_need_gc_pause XOA (scheme_get_thread_local_variables()->scheme_future_need_gc_pause_)
#define scheme_use_rtcall XOA (scheme_get_thread_local_variables()->scheme_use_rtcall_)
#define quick_stx XOA (scheme_get_thread_local_variables()->quick_stx_)
#define scheme_continuation_application_count XOA (scheme_get_thread_local_variables()->scheme_continuation_application_count_)
#define scheme_cont_capture_count XOA (scheme_get_thread_local_variables()->scheme_cont_capture_count_)
#define scheme_prompt_capture_count XOA (scheme_get_thread_local_variables()->scheme_prompt_capture_count_)
#define available_prompt XOA (scheme_get_thread_local_variables()->available_prompt_)
#define available_cws_prompt XOA (scheme_get_thread_local_variables()->available_cws_prompt_)
#define available_regular_prompt XOA (scheme_get_thread_local_variables()->available_regular_prompt_)
#define available_prompt_dw XOA (scheme_get_thread_local_variables()->available_prompt_dw_)
#define available_prompt_mc XOA (scheme_get_thread_local_variables()->available_prompt_mc_)
#define cached_beg_stx XOA (scheme_get_thread_local_variables()->cached_beg_stx_)
#define cached_dv_stx XOA (scheme_get_thread_local_variables()->cached_dv_stx_)
#define cached_ds_stx XOA (scheme_get_thread_local_variables()->cached_ds_stx_)
#define cached_stx_phase XOA (scheme_get_thread_local_variables()->cached_stx_phase_)
#define offstack_cont XOA (scheme_get_thread_local_variables()->offstack_cont_)
#define offstack_overflow XOA (scheme_get_thread_local_variables()->offstack_overflow_)
#define scheme_overflow_jmp XOA (scheme_get_thread_local_variables()->scheme_overflow_jmp_)
#define scheme_overflow_stack_start XOA (scheme_get_thread_local_variables()->scheme_overflow_stack_start_)
#define codetab_tree XOA (scheme_get_thread_local_variables()->codetab_tree_)
#define during_set XOA (scheme_get_thread_local_variables()->during_set_)
#define thread_local_pointers XOA (scheme_get_thread_local_variables()->thread_local_pointers_)
#define stack_cache_stack XOA (scheme_get_thread_local_variables()->stack_cache_stack_)
#define stack_cache_stack_pos XOA (scheme_get_thread_local_variables()->stack_cache_stack_pos_)
#define fixup_runstack_base XOA (scheme_get_thread_local_variables()->fixup_runstack_base_)
#define fixup_already_in_place XOA (scheme_get_thread_local_variables()->fixup_already_in_place_)
#define retry_alloc_r1 XOA (scheme_get_thread_local_variables()->retry_alloc_r1_)
#define save_fp XOA (scheme_get_thread_local_variables()->save_fp_)
#define starts_table XOA (scheme_get_thread_local_variables()->starts_table_)
#define modidx_caching_chain XOA (scheme_get_thread_local_variables()->modidx_caching_chain_)
#define global_shift_cache XOA (scheme_get_thread_local_variables()->global_shift_cache_)
#define proc_thread_self XOA (scheme_get_thread_local_variables()->proc_thread_self_)
#define scheme_orig_stdout_port XOA (scheme_get_thread_local_variables()->scheme_orig_stdout_port_)
#define scheme_orig_stderr_port XOA (scheme_get_thread_local_variables()->scheme_orig_stderr_port_)
#define scheme_orig_stdin_port XOA (scheme_get_thread_local_variables()->scheme_orig_stdin_port_)
#define scheme_fd_set XOA (scheme_get_thread_local_variables()->scheme_fd_set_)
#define new_port_cust XOA (scheme_get_thread_local_variables()->new_port_cust_)
#define external_event_fd XOA (scheme_get_thread_local_variables()->external_event_fd_)
#define put_external_event_fd XOA (scheme_get_thread_local_variables()->put_external_event_fd_)
#define read_string_byte_buffer XOA (scheme_get_thread_local_variables()->read_string_byte_buffer_)
#define itimerdata XOA (scheme_get_thread_local_variables()->itimerdata_)
#define quick_buffer XOA (scheme_get_thread_local_variables()->quick_buffer_)
#define quick_encode_buffer XOA (scheme_get_thread_local_variables()->quick_encode_buffer_)
#define cache_ht XOA (scheme_get_thread_local_variables()->cache_ht_)
#define regstr XOA (scheme_get_thread_local_variables()->regstr_)
#define regparsestr XOA (scheme_get_thread_local_variables()->regparsestr_)
#define regmatchmin XOA (scheme_get_thread_local_variables()->regmatchmin_)
#define regmatchmax XOA (scheme_get_thread_local_variables()->regmatchmax_)
#define regmaxbackposn XOA (scheme_get_thread_local_variables()->regmaxbackposn_)
#define regsavepos XOA (scheme_get_thread_local_variables()->regsavepos_)
#define regbackknown XOA (scheme_get_thread_local_variables()->regbackknown_)
#define regbackdepends XOA (scheme_get_thread_local_variables()->regbackdepends_)
#define regparse XOA (scheme_get_thread_local_variables()->regparse_)
#define regparse_end XOA (scheme_get_thread_local_variables()->regparse_end_)
#define regnpar XOA (scheme_get_thread_local_variables()->regnpar_)
#define regncounter XOA (scheme_get_thread_local_variables()->regncounter_)
#define regcode XOA (scheme_get_thread_local_variables()->regcode_)
#define regcodesize XOA (scheme_get_thread_local_variables()->regcodesize_)
#define regcodemax XOA (scheme_get_thread_local_variables()->regcodemax_)
#define regmaxlookback XOA (scheme_get_thread_local_variables()->regmaxlookback_)
#define rx_buffer_size XOA (scheme_get_thread_local_variables()->rx_buffer_size_)
#define startp_buffer_cache XOA (scheme_get_thread_local_variables()->startp_buffer_cache_)
#define endp_buffer_cache XOA (scheme_get_thread_local_variables()->endp_buffer_cache_)
#define maybep_buffer_cache XOA (scheme_get_thread_local_variables()->maybep_buffer_cache_)
#define scheme_os_thread_stack_base XOA (scheme_get_thread_local_variables()->scheme_os_thread_stack_base_)
#define traversers_registered XOA (scheme_get_thread_local_variables()->traversers_registered_)
#define save_fns_ptr XOA (scheme_get_thread_local_variables()->save_fns_ptr_)
#define scheme_system_idle_channel XOA (scheme_get_thread_local_variables()->scheme_system_idle_channel_)
#define system_idle_put_evt XOA (scheme_get_thread_local_variables()->system_idle_put_evt_)
#define stack_copy_cache XOA (scheme_get_thread_local_variables()->stack_copy_cache_)
#define stack_copy_size_cache XOA (scheme_get_thread_local_variables()->stack_copy_size_cache_)
#define scc_pos XOA (scheme_get_thread_local_variables()->scc_pos_)
#define prefab_table XOA (scheme_get_thread_local_variables()->prefab_table_)
#define nominal_ipair_cache XOA (scheme_get_thread_local_variables()->nominal_ipair_cache_)
#define mark_id XOA (scheme_get_thread_local_variables()->mark_id_)
#define current_rib_timestamp XOA (scheme_get_thread_local_variables()->current_rib_timestamp_)
#define quick_hash_table XOA (scheme_get_thread_local_variables()->quick_hash_table_)
#define last_phase_shift XOA (scheme_get_thread_local_variables()->last_phase_shift_)
#define unsealed_dependencies XOA (scheme_get_thread_local_variables()->unsealed_dependencies_)
#define id_marks_ht XOA (scheme_get_thread_local_variables()->id_marks_ht_)
#define than_id_marks_ht XOA (scheme_get_thread_local_variables()->than_id_marks_ht_)
#define interned_skip_ribs XOA (scheme_get_thread_local_variables()->interned_skip_ribs_)
#define scheme_current_thread XOA (scheme_get_thread_local_variables()->scheme_current_thread_)
#define scheme_main_thread XOA (scheme_get_thread_local_variables()->scheme_main_thread_)
#define scheme_first_thread XOA (scheme_get_thread_local_variables()->scheme_first_thread_)
#define scheme_thread_set_top XOA (scheme_get_thread_local_variables()->scheme_thread_set_top_)
#define num_running_threads XOA (scheme_get_thread_local_variables()->num_running_threads_)
#define swap_no_setjmp XOA (scheme_get_thread_local_variables()->swap_no_setjmp_)
#define thread_swap_count XOA (scheme_get_thread_local_variables()->thread_swap_count_)
#define scheme_did_gc_count XOA (scheme_get_thread_local_variables()->scheme_did_gc_count_)
#define scheme_future_state XOA (scheme_get_thread_local_variables()->scheme_future_state_)
#define scheme_future_thread_state XOA (scheme_get_thread_local_variables()->scheme_future_thread_state_)
#define jit_future_storage XOA (scheme_get_thread_local_variables()->jit_future_storage_)
#define scheme_current_runstack_start XOA (scheme_get_thread_local_variables()->scheme_current_runstack_start_)
#define scheme_current_runstack XOA (scheme_get_thread_local_variables()->scheme_current_runstack_)
#define scheme_current_cont_mark_stack XOA (scheme_get_thread_local_variables()->scheme_current_cont_mark_stack_)
#define scheme_current_cont_mark_pos XOA (scheme_get_thread_local_variables()->scheme_current_cont_mark_pos_)
#define main_custodian XOA (scheme_get_thread_local_variables()->main_custodian_)
#define last_custodian XOA (scheme_get_thread_local_variables()->last_custodian_)
#define limited_custodians XOA (scheme_get_thread_local_variables()->limited_custodians_)
#define swap_target XOA (scheme_get_thread_local_variables()->swap_target_)
#define scheduled_kills XOA (scheme_get_thread_local_variables()->scheduled_kills_)
#define do_atomic XOA (scheme_get_thread_local_variables()->do_atomic_)
#define missed_context_switch XOA (scheme_get_thread_local_variables()->missed_context_switch_)
#define have_activity XOA (scheme_get_thread_local_variables()->have_activity_)
#define scheme_active_but_sleeping XOA (scheme_get_thread_local_variables()->scheme_active_but_sleeping_)
#define thread_ended_with_activity XOA (scheme_get_thread_local_variables()->thread_ended_with_activity_)
#define scheme_no_stack_overflow XOA (scheme_get_thread_local_variables()->scheme_no_stack_overflow_)
#define needs_sleep_cancelled XOA (scheme_get_thread_local_variables()->needs_sleep_cancelled_)
#define tls_pos XOA (scheme_get_thread_local_variables()->tls_pos_)
#define the_nested_exn_handler XOA (scheme_get_thread_local_variables()->the_nested_exn_handler_)
#define cust_closers XOA (scheme_get_thread_local_variables()->cust_closers_)
#define thread_swap_callbacks XOA (scheme_get_thread_local_variables()->thread_swap_callbacks_)
#define thread_swap_out_callbacks XOA (scheme_get_thread_local_variables()->thread_swap_out_callbacks_)
#define recycle_cell XOA (scheme_get_thread_local_variables()->recycle_cell_)
#define maybe_recycle_cell XOA (scheme_get_thread_local_variables()->maybe_recycle_cell_)
#define recycle_cc_count XOA (scheme_get_thread_local_variables()->recycle_cc_count_)
#define main_init_error_buf XOA (scheme_get_thread_local_variables()->main_init_error_buf_)
#define gmp_mem_pool XOA (scheme_get_thread_local_variables()->gmp_mem_pool_)
#define max_total_allocation XOA (scheme_get_thread_local_variables()->max_total_allocation_)
#define current_total_allocation XOA (scheme_get_thread_local_variables()->current_total_allocation_)
#define gmp_tmp_xxx XOA (scheme_get_thread_local_variables()->gmp_tmp_xxx_)
#define gmp_tmp_current XOA (scheme_get_thread_local_variables()->gmp_tmp_current_)

/* **************************************** */

# define THREAD_LOCAL_DECL(x) /* empty */

#endif

/* **************************************** */

#endif
