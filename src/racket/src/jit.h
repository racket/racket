/*
  JIT limtations:

  1) See "About short-jump mode" below.

  2) Use jit_patchable_movi_p() when a constant needs to be
     visible to the GC.

  3) Immediate operands must be 32-bit values on x86_64, except with
     jit_movi, jit_sti, jit_ldi, jit_bXi, jit_calli, and jit_finishi.

  4) Function calls are limited to 3 arguments (i.e., jit_prepare()
     must never be called with a number greater than 3). This limit
     is related to the way the x86_64 port shuffles arguments into
     temporary registers.

  5) On non-Win64 x86_64, arguments are delivered in JIT_V2, JIT_V3,
     JIT_R2, and JIT_R1 in that order. So don't set JIT_R2 before
     getting the third argument, etc.
  
     On non-Win64 x86_64, arguments are delivered in JIT_R1, JIT_R2,
     and other registers. So don't set JIT_R2 before getting the
     second argument, etc.

*/

#ifdef __APPLE__
# define _CALL_DARWIN
#endif

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Waddress"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#endif

/* Separate JIT_PRECISE_GC lets us test some 3m support in non-3m mode: */
#ifdef MZ_PRECISE_GC
# define JIT_PRECISE_GC
#endif

/* IMPORTANT! 3m arithmetic checking disabled for the whole file! */
#ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
#endif

#define JIT_USE_FP_OPS

#ifdef MZ_USE_JIT_X86_64
# define MZ_USE_JIT_I386
# define JIT_X86_64
# ifndef MZ_NO_JIT_SSE
#  define JIT_X86_SSE
# endif
#endif

#ifdef MZ_USE_JIT_I386
# ifndef JIT_X86_64
#  define JIT_X86_PLAIN
# endif
#endif

#ifdef MZ_USE_JIT_SSE
# ifndef JIT_X86_SSE
#  define JIT_X86_SSE
# endif
#endif

#ifdef MZ_USE_JIT_PPC
# ifndef DEFINE_LIGHTNING_FUNCS
#  define SUPPRESS_LIGHTNING_FUNCS
# endif
# define DEFINE_LIGHTNING_FUNCS_STATIC /* empty */
# define jit_notify_freed_code scheme_jit_notify_freed_code
# define jit_flush_code scheme_jit_flush_code
# define _jit_prolog scheme_jit_prolog
# define _jit_epilog scheme_jit_epilog
#endif

#include "lightning/lightning.h"
#define _jit (jitter->js)

#ifdef MZ_USE_JIT_X86_64
# define JIT_LOG_WORD_SIZE 3
#else
# define JIT_LOG_WORD_SIZE 2
#endif
#define JIT_WORD_SIZE (1 << JIT_LOG_WORD_SIZE)
#define WORDS_TO_BYTES(x) ((x) << JIT_LOG_WORD_SIZE)
#define MAX_TRY_SHIFT 30

#ifdef USE_THREAD_LOCAL
# define NATIVE_ARG_COUNT 4
#else
# define NATIVE_ARG_COUNT 3
#endif

#define JIT_LOG_DOUBLE_SIZE 3
#define JIT_DOUBLE_SIZE (1 << JIT_LOG_DOUBLE_SIZE)

/* a mzchar is an int: */
#define LOG_MZCHAR_SIZE 2

#if defined(MZ_USE_JIT_PPC) || defined(MZ_USE_JIT_X86_64)
# define NEED_LONG_JUMPS
#endif
/* Tiny jumps seem worthwhile for x86, but they don't seem to help for x86_64: */
#if defined(MZ_USE_JIT_I386) && !defined(MZ_USE_JIT_X86_64)
# define USE_TINY_JUMPS
#endif

#if defined(MZ_PRECISE_GC) && defined(MZ_USE_JIT_I386)
# define USE_FLONUM_UNBOXING
#endif

#ifdef MZ_USE_FUTURES
# define MZ_USE_LWC
#endif

#define JIT_NOT_RET JIT_R1
#if JIT_NOT_RET == JIT_RET
Fix me! See use.
#endif

#ifdef MZ_USE_SINGLE_FLOATS
# define SCHEME_FLOAT_TYPE scheme_float_type
#else
# define SCHEME_FLOAT_TYPE scheme_double_type
#endif

#define NATIVE_PRESERVES_MARKS 0x1
#define NATIVE_IS_SINGLE_RESULT 0x2

#if defined(MZ_PRECISE_GC) && !defined(USE_COMPACT_3M_GC)
# define CAN_INLINE_ALLOC
#endif

#ifdef JIT_USE_FP_OPS
# define INLINE_FP_COMP
# ifdef CAN_INLINE_ALLOC
#  define INLINE_FP_OPS
# endif
#endif

#if 0
static void assert_failure(int where) { printf("JIT assert failed %d\n", where); }
#define JIT_ASSERT(v) if (!(v)) assert_failure(__LINE__);
#else
#define JIT_ASSERT(v) /* */
#endif

/* Tracking statistics: */
#if 0
# define NUM_CATEGORIES 23
extern int jit_sizes[NUM_CATEGORIES];
extern int jit_counts[NUM_CATEGORIES];
extern int jit_code_size;
# define START_JIT_DATA() void *__pos = jit_get_ip().ptr; uintptr_t __total = 0
# define END_JIT_DATA(where) if (jitter->retain_start) { \
                              jit_sizes[where] += __total + ((uintptr_t)jit_get_ip().ptr - (uintptr_t)__pos); \
                              jit_counts[where]++; }
# define PAUSE_JIT_DATA() __total += ((uintptr_t)jit_get_ip().ptr - (uintptr_t)__pos)
# define RESUME_JIT_DATA() __pos = jit_get_ip().ptr
# define RECORD_CODE_SIZE(s) jit_code_size += s
#else
# define START_JIT_DATA() /* empty */
# define END_JIT_DATA(where) /* empty */
# define PAUSE_JIT_DATA() /* empty */
# define RESUME_JIT_DATA() /* empty */
# define RECORD_CODE_SIZE(s) /* empty */
#endif

extern int scheme_direct_call_count, scheme_indirect_call_count;
extern int scheme_jit_malloced;
#ifdef JIT_USE_FP_OPS
THREAD_LOCAL_DECL(extern double scheme_jit_save_fp);
THREAD_LOCAL_DECL(extern double scheme_jit_save_fp2);
#endif

typedef int (*Native_Check_Arity_Proc)(Scheme_Object *o, int argc, int dummy EXTRA_NATIVE_ARGUMENT_TYPE);
typedef Scheme_Object *(*Native_Get_Arity_Proc)(Scheme_Object *o, int dumm1, int dummy2 EXTRA_NATIVE_ARGUMENT_TYPE);
typedef Scheme_Object *(*LWC_Native_Starter)(void *data,
                                             int argc,
                                             Scheme_Object **argv,
                                             void *thdloc,
                                             Scheme_Native_Proc *chain_to,
                                             void **save_pos);

typedef struct Apply_LWC_Args {
  void *dest_stack_pos; /* must be first */
  Scheme_Current_LWC *lwc;
  void *copy_to_install;
  intptr_t full_size, copy_size;
#ifdef JIT_X86_64
  intptr_t saved_r14, saved_r15;
# ifdef _WIN64
  intptr_t saved_r12, saved_r13;
# endif
#endif
  Scheme_Object *result;
  void *new_runstack;
  void *new_runstack_base;
  void *new_threadlocal;
} Apply_LWC_Args;

typedef Scheme_Object *(*Continuation_Apply_Indirect)(Apply_LWC_Args *, intptr_t);
typedef Scheme_Object *(*Continuation_Apply_Finish)(Apply_LWC_Args *args, void *stack, void *frame);

struct scheme_jit_common_record {
  int skip_checks;

#define MAX_SHARED_CALL_RANDS 25
  void *shared_tail_code[4][MAX_SHARED_CALL_RANDS];
  void *shared_non_tail_code[4][MAX_SHARED_CALL_RANDS][2];
  void *shared_non_tail_retry_code[2];
  void *shared_non_tail_argc_code[2];
  void *shared_tail_argc_code;

#define MAX_SHARED_ARITY_CHECK 25
  void *shared_arity_check[MAX_SHARED_ARITY_CHECK][2][2];

  void *bad_result_arity_code;
  void *unbound_global_code;
  void *quote_syntax_code;
  void *call_original_unary_arith_code;
  void *call_original_binary_arith_code;
  void *call_original_binary_rev_arith_code;
  void *call_original_unary_arith_for_branch_code;
  void *call_original_binary_arith_for_branch_code;
  void *call_original_binary_rev_arith_for_branch_code;
  void *call_original_nary_arith_code;
  void *bad_car_code, *bad_cdr_code;
  void *bad_caar_code, *bad_cdar_code, *bad_cadr_code, *bad_cddr_code;
  void *bad_cXr_code;
  void *bad_mcar_code, *bad_mcdr_code;
  void *bad_set_mcar_code, *bad_set_mcdr_code;
  void *imag_part_code, *real_part_code, *make_rectangular_code;
  void *bad_flimag_part_code, *bad_flreal_part_code, *bad_make_flrectangular_code;
  void *unbox_code, *set_box_code, *box_cas_fail_code;
  void *bad_vector_length_code;
  void *bad_flvector_length_code;
  void *bad_fxvector_length_code;
  void *vector_ref_code, *vector_ref_check_index_code, *vector_set_code, *vector_set_check_index_code;
  void *chap_vector_ref_code, *chap_vector_ref_check_index_code, *chap_vector_set_code, *chap_vector_set_check_index_code;
  void *string_ref_code, *string_ref_check_index_code, *string_set_code, *string_set_check_index_code;
  void *bytes_ref_code, *bytes_ref_check_index_code, *bytes_set_code, *bytes_set_check_index_code;
  void *flvector_ref_check_index_code, *flvector_set_check_index_code, *flvector_set_flonum_check_index_code;
  void *fxvector_ref_code, *fxvector_ref_check_index_code, *fxvector_set_code, *fxvector_set_check_index_code;
  void *struct_raw_ref_code, *struct_raw_set_code;
  void *syntax_e_code;
  void *on_demand_jit_arity_code, *in_progress_on_demand_jit_arity_code;
  void *get_stack_pointer_code;
  void *stack_cache_pop_code;
  void *struct_pred_code, *struct_pred_multi_code;
  void *struct_pred_branch_code;
  void *struct_get_code, *struct_get_multi_code;
  void *struct_set_code, *struct_set_multi_code;
  void *struct_prop_get_code, *struct_prop_get_multi_code;
  void *struct_prop_get_defl_code, *struct_prop_get_defl_multi_code;
  void *struct_prop_pred_code, *struct_prop_pred_multi_code;
  void *struct_proc_extract_code;
  void *bad_app_vals_target;
  void *app_values_slow_code, *app_values_multi_slow_code, *app_values_tail_slow_code;
  void *values_code;
  void *list_p_code, *list_p_branch_code;
  void *list_length_code;
  void *list_ref_code, *list_tail_code;
  void *finish_tail_call_code, *finish_tail_call_fixup_code;
  void *module_run_start_code, *module_exprun_start_code, *module_start_start_code;
  void *box_flonum_from_stack_code, *box_flonum_from_reg_code;
  void *fl1_fail_code, *fl2rr_fail_code[2], *fl2fr_fail_code[2], *fl2rf_fail_code[2];
  void *wcm_code, *wcm_nontail_code;
  void *apply_to_list_tail_code, *apply_to_list_code, *apply_to_list_multi_ok_code;
  void *eqv_code, *eqv_branch_code;
  void *proc_arity_includes_code;

#ifdef CAN_INLINE_ALLOC
  void *make_list_code, *make_list_star_code;
  void *retry_alloc_code;
  void *retry_alloc_code_keep_r0_r1;
  void *retry_alloc_code_keep_fpr1;
#endif

  Continuation_Apply_Indirect continuation_apply_indirect_code;
#ifdef MZ_USE_LWC
  Continuation_Apply_Finish continuation_apply_finish_code;
#endif

  Native_Check_Arity_Proc check_arity_code;
  Native_Get_Arity_Proc get_arity_code;

  LWC_Native_Starter native_starter_code;
};

extern struct scheme_jit_common_record scheme_jit_common;

#define sjc scheme_jit_common

typedef struct mz_jit_state {
  MZTAG_IF_REQUIRED
  GC_CAN_IGNORE jit_state js;
  char *limit;
  int extra_pushed, max_extra_pushed;
  int depth; /* the position of the closure's first value on the stack */
  int max_depth, max_tail_depth;
  int *mappings; /* For each element,
		    case 0x1 bit:
		    . 0 -> case 0x2 bit:
                    .        0 -> case rest bits:
                    .               0 -> save point
                    .               1 -> shift >>2 to get orig pushed count
                    .        1 -> shift >>4 to get arity for single orig pushed
                    .             shift >>2 to get flags
		    . 1 -> case 0x2 bit:
                    .        0 -> shift >>2 to get new (native) pushed
                    .        1 -> shift >>2 to get flonum stack pos */
  int num_mappings, mappings_size;
  int retained, retained_double;
  int need_set_rs;
  void **retain_start;
  double *retain_double_start;
  Scheme_Native_Closure_Data *retaining_data; /* poke when setting retain_start for generational GC */
  int local1_busy, pushed_marks;
  int log_depth;
  int self_pos, self_closure_size, self_toplevel_pos;
  int self_to_closure_delta, closure_to_args_delta;
  int closure_self_on_runstack;
  int example_argc, example_argv_delta;
  Scheme_Object **example_argv;
  void *self_restart_code;
  void *self_nontail_code;
  Scheme_Native_Closure *nc; /* for extract_globals and extract_closure_local, only */
  Scheme_Closure_Data *self_data;
  void *status_at_ptr;
  int r0_status, r1_status;
  void *patch_depth;
  int rs_virtual_offset;
  int unbox, unbox_depth;
  int flostack_offset, flostack_space;
  int self_restart_offset, self_restart_space;
} mz_jit_state;

mz_jit_state *scheme_clone_jitter(mz_jit_state *j);
void scheme_unclone_jitter(mz_jit_state *j, mz_jit_state *j_copy);

typedef int (*Generate_Proc)(mz_jit_state *j, void *data);

typedef struct {
  jit_insn *addr;
  char mode, kind;
} Branch_Info_Addr;

#define BRANCH_ADDR_FALSE 0
#define BRANCH_ADDR_TRUE  1

#define BRANCH_ADDR_BRANCH    0
#define BRANCH_ADDR_UCBRANCH  1
#define BRANCH_ADDR_MOVI      2

typedef struct {
  int include_slow;
  int non_tail, restore_depth, flostack, flostack_pos;
  int branch_short, true_needs_jump;
  int addrs_count, addrs_size;
  Branch_Info_Addr *addrs;
} Branch_Info;

#define mz_CURRENT_REG_STATUS_VALID() (jitter->status_at_ptr == _jit.x.pc)
#define mz_SET_REG_STATUS_VALID(v) (jitter->status_at_ptr = (v ? _jit.x.pc : 0))

#define mz_SET_R0_STATUS_VALID(v) (jitter->status_at_ptr = (v ? _jit.x.pc : 0), \
                                   jitter->r1_status = -1)

/* If JIT_THREAD_LOCAL is defined, then access to global variables
   goes through a thread_local_pointers table. Call
   scheme_jit_fill_threadlocal_table() to fill the table in a new
   OS-level thread. Use mz_tl_ldi_p(), etc., with `tl_MZ_RUNSTACK',
   etc., to access variables that can be thread local. (JIT-generated
   code accesses only a handful, so we can just enumerate them.)

   On x86, the thread-local table pointer is loaded on entry to the
   JIT world into a C stack slot. On x86_64, it is loaded into the
   callee-saved R14 (and the old value is saved on the C stack). */
#ifdef USE_THREAD_LOCAL
# define JIT_THREAD_LOCAL
#endif

#ifdef JIT_THREAD_LOCAL
# define tl_delta(id) ((uintptr_t)&(id) - (uintptr_t)&BOTTOM_VARIABLE)
# define tl_MZ_RUNSTACK                    tl_delta(MZ_RUNSTACK)
# define tl_MZ_RUNSTACK_START              tl_delta(MZ_RUNSTACK_START)
# define tl_GC_gen0_alloc_page_ptr         tl_delta(GC_gen0_alloc_page_ptr)
# define tl_scheme_current_thread          tl_delta(scheme_current_thread)
# define tl_scheme_current_cont_mark_pos   tl_delta(scheme_current_cont_mark_pos)
# define tl_scheme_current_cont_mark_stack tl_delta(scheme_current_cont_mark_stack)
# define tl_stack_cache_stack_pos          tl_delta(stack_cache_stack_pos)
# define tl_retry_alloc_r1                 tl_delta(retry_alloc_r1)
# define tl_fixup_runstack_base            tl_delta(fixup_runstack_base)
# define tl_fixup_already_in_place         tl_delta(fixup_already_in_place)
# define tl_scheme_jit_save_fp             tl_delta(scheme_jit_save_fp)
# define tl_scheme_jit_save_fp2            tl_delta(scheme_jit_save_fp2)
# define tl_scheme_fuel_counter            tl_delta(scheme_fuel_counter)
# define tl_scheme_jit_stack_boundary      tl_delta(scheme_jit_stack_boundary)
# define tl_jit_future_storage             tl_delta(jit_future_storage)
# define tl_scheme_future_need_gc_pause    tl_delta(scheme_future_need_gc_pause)
# define tl_scheme_use_rtcall              tl_delta(scheme_use_rtcall)
# define tl_scheme_current_lwc             tl_delta(scheme_current_lwc)

void *scheme_jit_get_threadlocal_table();

# ifdef JIT_X86_64
#  define JIT_R10 JIT_R(10)
#  define JIT_R14 JIT_R(14)
#  define mz_tl_addr(reg, addr) LEAQmQr((addr), (JIT_R14), 0, 0, (reg))
#  define mz_tl_addr_tmp(tmp_reg, addr) (mz_tl_addr(JIT_R10, addr))
#  define mz_tl_addr_untmp(tmp_reg) (void)0
#  define mz_tl_tmp_reg(tmp_reg) JIT_R10
#  define _mz_tl_str_p(addr, tmp_reg, reg) jit_str_p(tmp_reg, reg)
#  define _mz_tl_str_l(addr, tmp_reg, reg) jit_str_l(tmp_reg, reg)
#  define _mz_tl_str_i(addr, tmp_reg, reg) jit_str_i(tmp_reg, reg)
# else
#  define THREAD_LOCAL_USES_JIT_V2
#  ifdef THREAD_LOCAL_USES_JIT_V2
#   define mz_tl_addr(reg, addr) (jit_addi_p(reg, JIT_V2, addr))
#   define mz_tl_addr_tmp(tmp_reg, addr) (void)0
#   define mz_tl_addr_untmp(tmp_reg) 0
#   define mz_tl_tmp_reg(tmp_reg) (void)0
#   define _mz_tl_str_p(addr, tmp_reg, reg) jit_stxi_p(addr, JIT_V2, reg)
#   define _mz_tl_str_l(addr, tmp_reg, reg) jit_stxi_l(addr, JIT_V2, reg)
#   define _mz_tl_str_i(addr, tmp_reg, reg) jit_stxi_i(addr, JIT_V2, reg)
#  else
#   define mz_tl_addr(reg, addr) (mz_get_local_p(reg, JIT_LOCAL4), jit_addi_p(reg, reg, addr))
#   define mz_tl_addr_tmp(tmp_reg, addr) (PUSHQr(tmp_reg), mz_tl_addr(tmp_reg, addr))
#   define mz_tl_addr_untmp(tmp_reg) POPQr(tmp_reg)
#   define mz_tl_tmp_reg(tmp_reg) tmp_reg
#   define _mz_tl_str_p(addr, tmp_reg, reg) jit_str_p(tmp_reg, reg)
#   define _mz_tl_str_l(addr, tmp_reg, reg) jit_str_l(tmp_reg, reg)
#   define _mz_tl_str_i(addr, tmp_reg, reg) jit_str_i(tmp_reg, reg)
#  endif
# endif

/* A given tmp_reg doesn't have to be unused; it just has to be distinct from other arguments. */
# define mz_tl_sti_p(addr, reg, tmp_reg) (mz_tl_addr_tmp(tmp_reg, addr), _mz_tl_str_p(addr, mz_tl_tmp_reg(tmp_reg), reg), mz_tl_addr_untmp(tmp_reg))
# define mz_tl_sti_l(addr, reg, tmp_reg) (mz_tl_addr_tmp(tmp_reg, addr), _mz_tl_str_l(addr, mz_tl_tmp_reg(tmp_reg), reg), mz_tl_addr_untmp(tmp_reg))
# define mz_tl_sti_i(addr, reg, tmp_reg) (mz_tl_addr_tmp(tmp_reg, addr), _mz_tl_str_i(addr, mz_tl_tmp_reg(tmp_reg), reg), mz_tl_addr_untmp(tmp_reg))
# define mz_tl_ldi_p(reg, addr) (mz_tl_addr(reg, addr), jit_ldr_p(reg, reg))
# define mz_tl_ldi_l(reg, addr) (mz_tl_addr(reg, addr), jit_ldr_l(reg, reg))
# define mz_tl_ldi_i(reg, addr) (mz_tl_addr(reg, addr), jit_ldr_i(reg, reg))
# define mz_tl_sti_d_fppop(addr, reg, tmp_reg) (mz_tl_addr(tmp_reg, addr), jit_str_d_fppop(tmp_reg, reg))
# define mz_tl_ldi_d_fppush(reg, addr, tmp_reg) (mz_tl_addr(tmp_reg, addr), jit_ldr_d_fppush(reg, tmp_reg))
#else
# define mz_tl_sti_p(addr, reg, tmp_reg) jit_sti_p(addr, reg)
# define mz_tl_sti_l(addr, reg, tmp_reg) jit_sti_l(addr, reg)
# define mz_tl_sti_i(addr, reg, tmp_reg) jit_sti_i(addr, reg)
# define mz_tl_ldi_p(reg, addr) jit_ldi_p(reg, addr)
# define mz_tl_ldi_l(reg, addr) jit_ldi_l(reg, addr)
# define mz_tl_ldi_i(reg, addr) jit_ldi_i(reg, addr)
# define mz_tl_sti_d_fppop(addr, reg, tmp_reg) jit_sti_d_fppop(addr, reg)
# define mz_tl_ldi_d_fppush(reg, addr, tmp_reg) jit_ldi_d_fppush(reg, addr)
# define tl_MZ_RUNSTACK (&MZ_RUNSTACK)
# define tl_MZ_RUNSTACK_START (&MZ_RUNSTACK_START)
# define tl_GC_gen0_alloc_page_ptr (&GC_gen0_alloc_page_ptr)
# define tl_scheme_current_thread (&scheme_current_thread)
# define tl_scheme_current_cont_mark_pos (&scheme_current_cont_mark_pos)
# define tl_scheme_current_cont_mark_stack (&scheme_current_cont_mark_stack)
# define tl_stack_cache_stack_pos (&stack_cache_stack_pos)
# define tl_retry_alloc_r1 (&retry_alloc_r1)
# define tl_fixup_runstack_base (&fixup_runstack_base)
# define tl_fixup_already_in_place (&fixup_already_in_place)
# define tl_scheme_jit_save_fp (&scheme_jit_save_fp)
# define tl_scheme_jit_save_fp2 (&scheme_jit_save_fp2)
# define tl_scheme_fuel_counter (&scheme_fuel_counter)
# define tl_scheme_jit_stack_boundary (&scheme_jit_stack_boundary)
#endif

/*========================================================================*/
/*                           code-gen utils                               */
/*========================================================================*/

#define JIT_RUNSTACK JIT_V0

#ifndef THREAD_LOCAL_USES_JIT_V2
# define JIT_RUNSTACK_BASE JIT_V2
# define JIT_RUNSTACK_BASE_OR_ALT(alt) JIT_RUNSTACK_BASE
# define mz_ld_runstack_base_alt(reg) /* empty */
# define mz_st_runstack_base_alt(reg) /* empty */
#else
# define JIT_RUNSTACK_BASE_OR_ALT(alt) alt
# define JIT_RUNSTACK_BASE_LOCAL JIT_LOCAL4
# define mz_ld_runstack_base_alt(reg) mz_get_local_p(reg, JIT_RUNSTACK_BASE_LOCAL)
# define mz_st_runstack_base_alt(reg) mz_set_local_p(reg, JIT_RUNSTACK_BASE_LOCAL)
#endif

#define JIT_UPDATE_THREAD_RSPTR() mz_tl_sti_p(tl_MZ_RUNSTACK, JIT_RUNSTACK, JIT_R0)
#define JIT_UPDATE_THREAD_RSPTR_IF_NEEDED() \
    if (jitter->need_set_rs) {   \
      JIT_UPDATE_THREAD_RSPTR(); \
      jitter->need_set_rs = 0;   \
    }
#define JIT_UPDATE_THREAD_RSPTR_FOR_BRANCH_IF_NEEDED() \
    if (jitter->need_set_rs) {   \
      JIT_UPDATE_THREAD_RSPTR(); \
    }

#if 0
/* Debugging: checking for runstack overflow. A CHECK_RUNSTACK_OVERFLOW() should
   be included after each decrement of JIT_RUNSTACK. Failure is "reported" by
   going into an immediate loop. */
static void *top;
static void *cr_tmp;
# define CHECK_RUNSTACK_OVERFLOW_NOCL() \
     jit_sti_l(&cr_tmp, JIT_R0); jit_ldi_l(JIT_R0, &scheme_current_runstack_start); \
  top = (_jit.x.pc); (void)jit_bltr_ul(top, JIT_RUNSTACK, JIT_R0); jit_ldi_l(JIT_R0, &cr_tmp)
# define CHECK_RUNSTACK_OVERFLOW() \
     CHECK_LIMIT(); CHECK_RUNSTACK_OVERFLOW_NOCL()
#else
# define CHECK_RUNSTACK_OVERFLOW() /* empty */
# define CHECK_RUNSTACK_OVERFLOW_NOCL() /* empty */
#endif

#if 0
/* Debugging: ... */
static void *top4;
# define VALIDATE_RESULT(reg) top4 = (_jit.x.pc); (void)jit_beqi_ul(top4, reg, 0)
#else
# define VALIDATE_RESULT(reg) /* empty */
#endif

/* The mz_rs_... family of operations operate on a virtual
   JIT_RUNSTACK register to perform a kind of peephole optimization.
   The virtual register can be de-sync'd from the actual register, so
   that multiple adjustments to the register can be collapsed; this
   mostly improves code size, rather than speed. Functions that cause
   the register to be de-sync'd are marked as such. Functions that can
   accommodate a de-sync'd register on entry are marked as such. All
   other functions can assume a sync'd regsiter and ensure a sync'd
   register. Note that branches and calls normally require a sync'd
   register. */

#if 1
# define mz_rs_dec(n) (((jitter->r0_status >= 0) ? jitter->r0_status += (n) : 0), \
                       ((jitter->r1_status >= 0) ? jitter->r1_status += (n) : 0), \
                       jitter->rs_virtual_offset -= (n))
# define mz_rs_inc(n) (jitter->r0_status -= (n), \
                       jitter->r1_status -= (n), \
                       jitter->rs_virtual_offset += (n))
# define mz_rs_ldxi(reg, n) jit_ldxi_p(reg, JIT_RUNSTACK, WORDS_TO_BYTES(((n) + jitter->rs_virtual_offset)))
# define mz_rs_ldr(reg) mz_rs_ldxi(reg, 0)
# define mz_rs_stxi(n, reg) jit_stxi_p(WORDS_TO_BYTES(((n) + jitter->rs_virtual_offset)), JIT_RUNSTACK, reg)
# define mz_rs_str(reg) mz_rs_stxi(0, reg)
# define mz_rs_sync() (jitter->rs_virtual_offset \
                       ? ((jitter->status_at_ptr == _jit.x.pc) \
                          ? (jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(jitter->rs_virtual_offset)), \
                             jitter->status_at_ptr = _jit.x.pc, \
                             jitter->rs_virtual_offset = 0) \
                          : (jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(jitter->rs_virtual_offset)), \
                             jitter->rs_virtual_offset = 0)) \
                       : 0)
# define mz_rs_sync_0() (jitter->rs_virtual_offset = 0)
#else
# define mz_rs_dec(n) jit_subi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(n))
# define mz_rs_inc(n) jit_addi_p(JIT_RUNSTACK, JIT_RUNSTACK, WORDS_TO_BYTES(n))
# define mz_rs_ldr(reg) jit_ldr_p(reg, JIT_RUNSTACK)
# define mz_rs_ldxi(reg, n) jit_ldxi_p(reg, JIT_RUNSTACK, WORDS_TO_BYTES(n))
# define mz_rs_str(reg) jit_str_p(JIT_RUNSTACK, reg)
# define mz_rs_stxi(n, reg) jit_stxi_p(WORDS_TO_BYTES(n), JIT_RUNSTACK, reg)
# define mz_rs_sync() /* empty */
# define mz_rs_sync_0() /* empty */
#endif

/* No need to sync if a branch just goes to an exception. */
# define mz_rs_sync_fail_branch() /* empty */

/* de-sync's rs: */
#define mz_pushr_p(x) scheme_mz_pushr_p_it(jitter, x)
#define mz_popr_p(x) scheme_mz_popr_p_it(jitter, x, 0)
#define mz_popr_x() scheme_mz_popr_p_it(jitter, JIT_R1, 1)

#if 0
/* Debugging: at each _finish(), double-check that the runstack register has been
   copied into scheme_current_runstack. This code assumes that mz_finishr() is not
   used with JIT_R0.  Failure is "reported" by going into an immediate loop, but
   check_location is set to the source line number to help indicate where the
   problem originated. */
static void *top;
int check_location;
# define CONFIRM_RUNSTACK() (jit_movi_l(JIT_R0, __LINE__), jit_sti_l(&check_location, JIT_R0), \
                             mz_tl_ldi_p(JIT_R0, tl_MZ_RUNSTACK), top = (_jit.x.pc), jit_bner_p(top, JIT_RUNSTACK, JIT_R0))
#else
# define CONFIRM_RUNSTACK() 0
#endif

#define mz_prepare(x) jit_prepare(x)
#define mz_finish(x) ((void)CONFIRM_RUNSTACK(), jit_finish(x))
#define mz_finishr(x) ((void)CONFIRM_RUNSTACK(), jit_finishr(x))

#define mz_nonrs_finish(x) jit_finish(x)

#define mz_retain(x) scheme_mz_retain_it(jitter, x)
#define mz_remap(x) scheme_mz_remap_it(jitter, x)

#ifdef jit_bxnei_s
# define mz_bnei_t(label, reg, stype, scratch_reg) jit_bxnei_s(label, reg, stype)
# define mz_beqi_t(label, reg, stype, scratch_reg) jit_bxeqi_s(label, reg, stype)
#else
# define mz_bnei_t(label, reg, stype, scratch_reg) \
  (jit_ldxi_s(scratch_reg, reg, &((Scheme_Object *)0x0)->type), \
   jit_bnei_i(label, scratch_reg, stype))
# define mz_beqi_t(label, reg, stype, scratch_reg) \
  (jit_ldxi_s(scratch_reg, reg, &((Scheme_Object *)0x0)->type), \
   jit_beqi_i(label, scratch_reg, stype))
#endif

/* Stack alignment, fixed up by mz_push_locals():
    - On PPC, jit_prolog() generates an aligned stack.
      It also leaves room for 3 locals.
    - On x86, jit_prolog() pushes three words after the
      old EBP. So, for 16-byte alignment, the stack is
      one word past proper alignment; push 3 to realign
      (which leaves room for three locals)
    - On x86_64, jit_prolog() pushes three words after
      the old RBP. So, for 16-byte alignment, the stack
      is one word past alignment. Push 1 to realign (but
      mz_push_locals() pushes 3, because we need at least
      two locals). 
*/

/*    LOCAL1 is used to save the value current_cont_mark_stack,
      at least for the first time it needs to be saved in a
      function body. If it needs to be saved again, it is
      pushed onto the runstack. (The value of current_cont_mark_stack
      is an integer that marks a point in the stack, as opposed
      to being an address of a stack position.) */

/* 
   mz_prolog() and mz_epilog() bracket an internal "function" using a
   lighter-weight ABI that keeps all Rx and Vx registers as-is on
   entry and exit, as well as the frame pointer. Some of those
   functions are registered in a special way with add_symbol() so that
   the backtrace function can follow the lightweight ABI to get back
   to the calling code. The lightweight ABI does not support nested
   calls (at least not on all platforms; see LOCAL2 below).

   LOCAL2 and LOCAL3 are available for temporary storage on the C
   stack using mz_get_local() and mz_set_local() under certain
   circumstances:

   * They can only be used within a function (normally corresponding
     to a Racket lambda) where mz_push_locals() has been called after
     jit_prolog(), and where mz_pop_locals() is called before
     jit_ret().

   * On some platforms, LOCAL2 and LOCAL3 are the same.

   * On some platforms, a lightweight function created with
     mz_prolog() and mz_epilog() uses LOCAL2 to save the return
     address. On those platforms, though, LOCAL3 is dufferent from
     LOCAL2. So, LOCAL3 can always be used for temporary storage in
     such functions (assuming that they're called from a function that
     pushes locals, and that nothing else is using LOCAL2).

*/

/*  x86[_64] frame (counting down from frame pointer marked with <-):
      return address
      prev frame <-
      saved EBX (= JIT_RUNSTACK, when saved from native call)
      saved R12/ESI (= JIT_V1, when saved from native call)
      saved R13/EDI (= JIT_V2 x86_64: = RUNSTACK_BASE, when saved from native call
                              x86: = THREAD_LOCAL or RUNSTACK_BASE, when saved from native call
      LOCAL1 (which is a cont_mark_stack offset, if anything)
      LOCAL2 (some pointer, never to stack or runstack)
      LOCAL3 (temp space for misc uses; not saved across calls that might capture LWC)
      LOCAL4 (x86_64: = saved R14 otherwise when THREAD_LOCAL
              x86: = RUNSTACK_BASE or THREAD_LOCAL)
      [some empty slots, maybe, depending on alignment]
      [space for local, unboxed flonums]
    Registers: JIT_V1 = RUNSTACK, JIT_V2 = x86_64: RUNSTACK_BASE
                                           x86: RUNSTACK_BASE or THREAD_LOCAL
               x86_64: JIT_R14 = THREAD_LOCAL
*/

#ifdef JIT_THREAD_LOCAL
# define NEED_LOCAL4
#endif

#ifdef MZ_USE_JIT_PPC
/* JIT_LOCAL1, JIT_LOCAL2, and JIT_LOCAL3 are offsets in the stack frame. */
# define JIT_LOCAL1 56
# define JIT_LOCAL2 60
# define JIT_LOCAL3 64
# define mz_set_local_p(x, l) jit_stxi_p(l, JIT_FP, x)
# define mz_get_local_p(x, l) jit_ldxi_p(x, JIT_FP, l)
# define mz_patch_branch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_branch(a-1, v))
# define mz_patch_ucbranch_at(a, v) (_jitl.long_jumps ? (void)jit_patch_movei(a-4, a-3, v) : (void)jit_patch_ucbranch(a-1, v))
# define mz_prolog(x) (MFLRr(x), mz_set_local_p(x, JIT_LOCAL2))
# define mz_epilog(x) (mz_get_local_p(x, JIT_LOCAL2), jit_jmpr(x))
# define mz_epilog_without_jmp() /* empty */
# define jit_shuffle_saved_regs() /* empty */
# define jit_unshuffle_saved_regs() /* empty */
# define mz_push_locals() /* empty */
# define mz_pop_locals() /* empty */
# ifdef SUPPRESS_LIGHTNING_FUNCS
void scheme_jit_prolog_again(mz_jit_state *jitter, int n, int ret_addr_reg);
# else
void scheme_jit_prolog_again(mz_jit_state *jitter, int n, int ret_addr_reg)
{
  /* This must be consistent with _jit_prolog in many ways: */
  int frame_size;
  int ofs;
  int first_saved_reg = JIT_AUX - n;
  int num_saved_regs = 32 - first_saved_reg;

  frame_size = 24 + 32 + 12 + num_saved_regs * 4;	/* r27..r31 + args		   */
  frame_size += 15;			/* the stack must be quad-word     */
  frame_size &= ~15;			/* aligned			   */

  STWUrm(1, -frame_size, 1);		/* stwu  r1, -x(r1)		   */

  /* We actually only need to save V0-V2, which are at
     the end of the saved area: */
  first_saved_reg = 29;
  num_saved_regs = 3;

  ofs = frame_size - num_saved_regs * 4;
  STMWrm(first_saved_reg, ofs, 1);		/* stmw  rI, ofs(r1)		   */
#ifdef _CALL_DARWIN
  STWrm(ret_addr_reg, frame_size + 8, 1); /* stw   r0, x+8(r1)		   */
#else
  STWrm(ret_addr_reg, frame_size + 4, 1); /* stw   r0, x+4(r1)		   */
#endif
}
# endif
# define _jit_prolog_again scheme_jit_prolog_again
#else
/* From frame pointer, -1 is saved frame pointer, -2 is saved ESI/R12,
   and -3 is saved EDI/R13. On entry to a procedure, prolog pushes 4
   since the call (which also pushed), so if the stack was 16-bytes
   aligned before the call, it is current stack pointer is 1 word
   (either 4 or 8 bytes) below alignment (need to push 3 or 1 words to
   re-align). Also, for a call without a prolog, the stack pointer is
   1 word (for the return address) below alignment. */
# define JIT_LOCAL1 -(JIT_WORD_SIZE * 4)
# define JIT_LOCAL2 -(JIT_WORD_SIZE * 5)
# define mz_set_local_p(x, l) jit_stxi_p((l), JIT_FP, (x))
# define mz_get_local_p(x, l) jit_ldxi_p((x), JIT_FP, (l))
# define mz_patch_branch_at(a, v) jit_patch_branch_at(a, v)
# define mz_patch_ucbranch_at(a, v) jit_patch_ucbranch_at(a, v)
  /* The ABI for _CALL_DARWIN or JIT_X86_64 requires alignment. Even
     when it's not required, it's better for performance when flonums
     are stored on the stack. */
# define X86_ALIGN_STACK 1
# ifdef X86_ALIGN_STACK
   /* Maintain 16-byte stack alignment. */
#  ifdef JIT_X86_64
#   define STACK_ALIGN_WORDS 1
#  else
#   define STACK_ALIGN_WORDS 3
#  endif
#  define mz_prolog(x) (ADDQiBr(-(STACK_ALIGN_WORDS * JIT_WORD_SIZE), JIT_SP))
#  define mz_epilog_without_jmp() ADDQiBr((STACK_ALIGN_WORDS + 1) * JIT_WORD_SIZE, JIT_SP)
#  define mz_epilog(x) (ADDQiBr(STACK_ALIGN_WORDS * JIT_WORD_SIZE, JIT_SP), RET_())
#  define JIT_LOCAL3 -(JIT_WORD_SIZE * 6)
#  ifdef NEED_LOCAL4
#   ifdef JIT_X86_64
#    define LOCAL_FRAME_SIZE 5
#   else
#    define LOCAL_FRAME_SIZE 7
#   endif
#   define JIT_LOCAL4_OFFSET 7
#  else
#   define LOCAL_FRAME_SIZE 3
#  endif
# else
#  define mz_prolog(x) /* empty */
#  define mz_epilog(x) RET_()
#  define mz_epilog_without_jmp() ADDQir(JIT_WORD_SIZE, JIT_SP)
#  define JIT_LOCAL3 JIT_LOCAL2
#  ifdef NEED_LOCAL4
#   define LOCAL_FRAME_SIZE 3
#   define JIT_LOCAL4_OFFSET 6
#  else
#   define LOCAL_FRAME_SIZE 2
#  endif
# endif
# ifdef NEED_LOCAL4
#   define JIT_LOCAL4 -(JIT_WORD_SIZE * JIT_LOCAL4_OFFSET)
# endif
# define mz_push_locals() SUBQir((LOCAL_FRAME_SIZE << JIT_LOG_WORD_SIZE), JIT_SP)
# define mz_pop_locals() ADDQir((LOCAL_FRAME_SIZE << JIT_LOG_WORD_SIZE), JIT_SP)
# define JIT_FRAME_FLONUM_OFFSET (-(JIT_WORD_SIZE * (LOCAL_FRAME_SIZE + 3)))
# define _jit_prolog_again(jitter, n, ret_addr_reg) (PUSHQr(ret_addr_reg), jit_base_prolog())
# if defined(MZ_USE_JIT_X86_64) && !defined(_WIN64)
#  define jit_shuffle_saved_regs() (MOVQrr(_ESI, _R12), MOVQrr(_EDI, _R13))
#  define jit_unshuffle_saved_regs() (MOVQrr(_R12, _ESI), MOVQrr(_R13, _EDI))
# else
#  define jit_shuffle_saved_regs() /* empty */
#  define jit_unshuffle_saved_regs() /* empty */
# endif
#endif

#ifdef JIT_THREAD_LOCAL
# ifdef JIT_X86_64
#  define mz_pop_threadlocal() mz_get_local_p(JIT_R14, JIT_LOCAL4)
#  define mz_push_threadlocal(in) /* empty */
#  define mz_push_threadlocal_early() (mz_set_local_p(JIT_R14, JIT_LOCAL4), jit_movr_p(JIT_R14, JIT_R_ARG4))
#  define mz_repush_threadlocal() mz_set_local_p(JIT_R14, JIT_LOCAL4)
# else
#  define mz_pop_threadlocal() /* empty */
#  ifdef THREAD_LOCAL_USES_JIT_V2
#   define _mz_install_threadlocal(reg) jit_movr_p(JIT_V2, reg)
#   define mz_repush_threadlocal() /* empty */
#  else
#   define _mz_install_threadlocal(reg) mz_set_local_p(reg, JIT_LOCAL4)
#   define mz_repush_threadlocal() (PUSHQr(JIT_R0), jit_ldr_p(JIT_R0, _EBP), \
                                    jit_ldxi_p(JIT_R0, JIT_R0, JIT_LOCAL4), \
                                    jit_stxi_p(JIT_LOCAL4, _EBP, JIT_R0), \
                                    POPQr(JIT_R0))
#  endif
#  define mz_push_threadlocal(in) (in = jit_arg_p(), jit_getarg_p(JIT_V2, in), _mz_install_threadlocal(JIT_V2))
#  define mz_push_threadlocal_early() /* empty */
# endif
#else
# define mz_pop_threadlocal() /* empty */
# define mz_push_threadlocal(in) /* empty */
# define mz_push_threadlocal_early() /* empty */
# define mz_repush_threadlocal() /* empty */
#endif

#define mz_patch_branch(a) mz_patch_branch_at(a, (_jit.x.pc))
#define mz_patch_ucbranch(a) mz_patch_ucbranch_at(a, (_jit.x.pc))

#ifdef NEED_LONG_JUMPS
# define __START_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps = 0; }
# define __END_SHORT_JUMPS__(cond) if (cond) { _jitl.long_jumps= 1; }
#else
# define __START_SHORT_JUMPS__(cond) /* empty */
# define __END_SHORT_JUMPS__(cond) /* empty */
#endif

#ifdef USE_TINY_JUMPS
/* A tiny jump has to be between -128 and 127 bytes. */
# define __START_TINY_JUMPS__(cond) if (cond) { __START_SHORT_JUMPS__(1); _jitl.tiny_jumps = 1; }
# define __END_TINY_JUMPS__(cond) if (cond) { _jitl.tiny_jumps = 0; __END_SHORT_JUMPS__(1); }
# define __START_INNER_TINY__(cond) __END_SHORT_JUMPS__(cond); __START_TINY_JUMPS__(1);
# define __END_INNER_TINY__(cond) __END_TINY_JUMPS__(1); __START_SHORT_JUMPS__(cond); 
#else
# define __START_TINY_JUMPS__(cond) __START_SHORT_JUMPS__(cond)
# define __END_TINY_JUMPS__(cond) __END_SHORT_JUMPS__(cond)
# define __START_INNER_TINY__(cond) /* empty */
# define __END_INNER_TINY__(cond) /* empty */
#endif

#define __START_TINY_OR_SHORT_JUMPS__(tcond, cond) if (tcond) { __START_TINY_JUMPS__(1); } else { __START_SHORT_JUMPS__(cond); }
#define __END_TINY_OR_SHORT_JUMPS__(tcond, cond) if (tcond) { __END_TINY_JUMPS__(1); } else { __END_SHORT_JUMPS__(cond); }

#if defined(JIT_X86_64) || defined(JIT_X86_SSE)
# define __START_TINY_JUMPS_IF_COMPACT__(cond) /* empty */
# define __END_TINY_JUMPS_IF_COMPACT__(cond) /* empty */
#else
# define __START_TINY_JUMPS_IF_COMPACT__(cond) __START_TINY_JUMPS__(cond)
# define __END_TINY_JUMPS_IF_COMPACT__(cond) __END_TINY_JUMPS__(cond)
#endif

/* mz_b..i_p supports 64-bit constants on x86_64: */
#ifdef MZ_USE_JIT_X86_64
# define mz_beqi_p(a, v, i) ((void)jit_patchable_movi_p(JIT_REXTMP, i), jit_beqr_p(a, v, JIT_REXTMP))
# define mz_bnei_p(a, v, i) ((void)jit_patchable_movi_p(JIT_REXTMP, i), jit_bner_p(a, v, JIT_REXTMP))
#else
# define mz_beqi_p(a, v, i) jit_beqi_p(a, v, i)
# define mz_bnei_p(a, v, i) jit_bnei_p(a, v, i)
#endif

/* 
 About short-jump mode:
   
   In
      jit_jmpi(code);
   or
      jit_blti_i(code, v);
   the generated instructions can depend on the relative location
   between the instruction address and the actual value. Do not enable
   short jumps if the relative offset can change between the initial
   sizing pass and the final pass. Of course, also don't enable short
   jumps if the jump is potentially long (i.e. more than +/- 2^15
   on PowerPC, or more than +/- 2^31 on x86_64). Otherwise, enable
   short-jump mode as much as possible.

   Tiny-jump mode is like short-jump mode, but the offset must be
   within +/- 2^7. Favor tiny jumps over short jumps when possible.

   All mz_finish() and jit_calli() are implicitly long jumps.
*/

/* A lightweight continuation is one that contains only frames from
   JIT-generated code. Use scheme_call_as_lightweight_continuation()
   to start such a continuation, and it must be exited from the JIT
   world by mz_finish_lwe().

   Use mz_finish_lwe(addr, tmp) for a call that may capture a lightweight
   continuation:

   * JIT_V1 does not contain a value that needs to change if the runstack moves.
     (Other JIT constraints imply that it isn't a pointer to GCable memory.)

   * Relevant thread-local state is confined to the C stack, runstack,
     mark stack, and tl_save_fp[2].

   * A pointer to the runstack can be used as a Scheme_Object** argument, but
     only when it points to MZ_RUNSTACK.

  The `tmp' is a `jit_insn *' that can be used by the expansion of the
  macro.

*/

#ifdef MZ_USE_LWC
# ifdef JIT_RUNSTACK_BASE
#  define SAVE_RS_BASE_REG() jit_stxi_p((int)&((Scheme_Current_LWC *)0x0)->runstack_base_end, JIT_R0, JIT_RUNSTACK_BASE)
# else
#  define SAVE_RS_BASE_REG() (void)0
# endif
# define adjust_lwc_return_address(pc) ((jit_insn *)((char *)(pc) - jit_return_pop_insn_len()))
# define mz_finish_lwe(d, refr) (mz_tl_ldi_p(JIT_R0, tl_scheme_current_lwc), \
                                 jit_stxi_p((int)&((Scheme_Current_LWC *)0x0)->frame_end, JIT_R0, JIT_FP), \
                                 jit_stxi_p((int)&((Scheme_Current_LWC *)0x0)->stack_end, JIT_R0, JIT_SP), \
                                 jit_stxi_p((int)&((Scheme_Current_LWC *)0x0)->saved_v1, JIT_R0, JIT_V1), \
                                 SAVE_RS_BASE_REG(),                    \
                                 refr = jit_patchable_movi_p(JIT_R1, jit_forward()), \
                                 jit_stxi_p((int)&((Scheme_Current_LWC *)0x0)->original_dest, JIT_R0, JIT_R1), \
                                 mz_finish(d),                          \
                                 jit_patch_movi(refr, adjust_lwc_return_address(_jit.x.pc)))
#else
# define mz_finish_lwe(d, refr) (refr = NULL, mz_finish(d))
#endif

#define mz_nonrs_finish_lwe(d, refr) mz_finish_lwe(d, refr)

#if 0
# define FOR_LOG(x) x
# define LOG_IT(args) if (jitter->retain_start) { if (getenv("JITLOG")) { START_XFORM_SKIP; emit_indentation(jitter); printf args; END_XFORM_SKIP; } }
static void emit_indentation(mz_jit_state *jitter)
{
  int i = jitter->log_depth;
  while (i--) {
    printf("  ");
  }
}
#else
# define FOR_LOG(x) /* empty */
# define LOG_IT(args) /* empty */
#endif

/**********************************************************************/

/* FP-generation code is written to work both with a FP
   stack (i387) and normal FP regsiters (everything else), though the
   double-agent operations that end in _fppop() and _fppush(). In
   FP-stack mode, the register names don't actually matter, but the
   pushes and pops much balance. The popping branch operations pop
   both arguments before branching. */

#if !defined(MZ_USE_JIT_I386) || defined(JIT_X86_SSE)
/* Not FP stack, so use normal variants. */
#define DIRECT_FPR_ACCESS
#define jit_movi_d_fppush(rd,immd)    jit_movi_d(rd,immd)
#define jit_ldi_d_fppush(rd, is)      jit_ldi_d(rd, is)
#define jit_ldr_d_fppush(rd, rs)      jit_ldr_d(rd, rs)
#define jit_ldxi_d_fppush(rd, rs, is) jit_ldxi_d(rd, rs, is)
#define jit_ldxr_d_fppush(rd, rs, is) jit_ldxr_d(rd, rs, is)
#define jit_addr_d_fppop(rd,s1,s2)    jit_addr_d(rd,s1,s2)
#define jit_subr_d_fppop(rd,s1,s2)    jit_subr_d(rd,s1,s2)
#define jit_subrr_d_fppop(rd,s1,s2)   jit_subrr_d(rd,s1,s2)
#define jit_mulr_d_fppop(rd,s1,s2)    jit_mulr_d(rd,s1,s2)
#define jit_divr_d_fppop(rd,s1,s2)    jit_divr_d(rd,s1,s2)
#define jit_divrr_d_fppop(rd,s1,s2)   jit_divrr_d(rd,s1,s2)
#define jit_negr_d_fppop(rd,rs)       jit_negr_d(rd,rs)
#define jit_abs_d_fppop(rd,rs)        jit_abs_d(rd,rs)
#define jit_sqrt_d_fppop(rd,rs)       jit_sqrt_d(rd,rs)
#define jit_sti_d_fppop(id, rs)       jit_sti_d(id, rs)
#define jit_str_d_fppop(id, rd)       jit_str_d(id, rd)
#define jit_stxi_d_fppop(id, rd, rs)  jit_stxi_d(id, rd, rs)
#define jit_stxr_d_fppop(id, rd, rs)  jit_stxr_d(id, rd, rs)
#define jit_bger_d_fppop(d, s1, s2)   jit_bger_d(d, s1, s2)
#define jit_bantiger_d_fppop(d, s1, s2) jit_bantiger_d(d, s1, s2)
#define jit_bler_d_fppop(d, s1, s2)   jit_bler_d(d, s1, s2)
#define jit_bantiler_d_fppop(d, s1, s2) jit_bantiler_d(d, s1, s2)
#define jit_bgtr_d_fppop(d, s1, s2)   jit_bgtr_d(d, s1, s2)
#define jit_bantigtr_d_fppop(d, s1, s2) jit_bantigtr_d(d, s1, s2)
#define jit_bltr_d_fppop(d, s1, s2)   jit_bltr_d(d, s1, s2)
#define jit_bantiltr_d_fppop(d, s1, s2) jit_bantiltr_d(d, s1, s2)
#define jit_beqr_d_fppop(d, s1, s2)   jit_beqr_d(d, s1, s2)
#define jit_bantieqr_d_fppop(d, s1, s2) jit_bantieqr_d(d, s1, s2)
#define jit_extr_l_d_fppush(rd, rs)   jit_extr_l_d(rd, rs)
#define jit_roundr_d_l_fppop(rd, rs)  jit_roundr_d_l(rd, rs)
#define jit_movr_d_rel(rd, rs)        jit_movr_d(rd, rs)
#define jit_movr_d_fppush(rd, rs)        jit_movr_d(rd, rs)
#define R0_FP_ADJUST(x) /* empty */
#define JIT_FPR_0(r) JIT_FPR(r)
#define JIT_FPR_1(r) JIT_FPR(r)
#else
#define R0_FP_ADJUST(x) x
#define JIT_FPR_0(r) JIT_FPR0
#define JIT_FPR_1(r) JIT_FPR1
#endif

#if defined(MZ_USE_JIT_I386)
# define mz_movi_d_fppush(rd,immd,tmp)    { GC_CAN_IGNORE void *addr; \
                                            addr = scheme_mz_retain_double(jitter, immd); \
                                            (void)jit_patchable_movi_p(tmp, addr);        \
                                            jit_ldr_d_fppush(rd, tmp); }
#else
# define mz_movi_d_fppush(rd,immd,tmp)    jit_movi_d_fppush(rd,immd)    
#endif

/**********************************************************************/

#ifdef MZ_USE_FUTURES
# define mz_prepare_direct_prim(n) mz_prepare(n)
# define mz_finishr_direct_prim(reg, proc, refr) (jit_pusharg_p(reg), (void)mz_finish_lwe(proc, refr))
# define mz_direct_only(p) /* skip this arg, so that total count <= 3 args */
/* Inlines check of scheme_use_rtcall: */
# define mz_generate_direct_prim(direct_only, first_arg, reg, prim_indirect) \
  { \
     GC_CAN_IGNORE jit_insn *refdirect, *refcont, *refitsr;      \
     int argstate; \
     jit_save_argstate(argstate); \
     mz_tl_ldi_i(JIT_R0, tl_scheme_use_rtcall); \
     __START_TINY_JUMPS__(1); \
     refdirect = jit_beqi_i(jit_forward(), JIT_R0, 0); \
     first_arg; \
     mz_finishr_direct_prim(reg, prim_indirect, refitsr);       \
     refcont = jit_jmpi(jit_forward()); \
     CHECK_LIMIT(); \
     mz_patch_branch(refdirect); \
     jit_restore_argstate(argstate); \
     direct_only; \
     first_arg; \
     mz_finishr(reg); \
     mz_patch_ucbranch(refcont); \
     __END_TINY_JUMPS__(1); \
  }
# define mz_finish_prim_lwe(prim, refr) \
    { \
      GC_CAN_IGNORE jit_insn *refdirect, *refdone; \
      int argstate; \
      __START_TINY_JUMPS__(1); \
      jit_save_argstate(argstate); \
      mz_tl_ldi_i(JIT_R0, tl_scheme_use_rtcall); \
      refdirect = jit_beqi_i(jit_forward(), JIT_R0, 0); \
      (void)mz_finish_lwe(prim, refr); \
      refdone = jit_jmpi(jit_forward()); \
      jit_restore_argstate(argstate); \
      mz_patch_branch(refdirect); \
      (void)mz_finish(prim); \
      mz_patch_ucbranch(refdone); \
      __END_TINY_JUMPS__(1); \
    }
#else
/* futures not enabled */
# define mz_prepare_direct_prim(n) mz_prepare(n)
# define mz_finishr_direct_prim(reg, proc) mz_finishr(reg)
# define mz_direct_only(p) p
# define ts_scheme_on_demand scheme_on_demand
# define ts_prepare_retry_alloc prepare_retry_alloc 
# define ts_make_fsemaphore scheme_make_fsemaphore
# define mz_generate_direct_prim(direct_only, first_arg, reg, prim_indirect) \
  (mz_direct_only(direct_only), first_arg, mz_finishr_direct_prim(reg, prim_indirect))
# define mz_finish_prim_lwe(prim, refr) (void)mz_finish_lwe(prim, refr)
#endif

/**********************************************************************/

#define IS_NAMED_PRIM(p, nm) (!strcmp(((Scheme_Primitive_Proc *)p)->name, nm))

/**********************************************************************/
/*                             jitstate                               */
/**********************************************************************/

#ifdef SIXTY_FOUR_BIT_INTEGERS
# define JIT_BUFFER_PAD_SIZE 200
#else
# define JIT_BUFFER_PAD_SIZE 100
#endif

#define PAST_LIMIT() ((uintptr_t)jit_get_ip().ptr > (uintptr_t)jitter->limit)
#define CHECK_LIMIT() if (PAST_LIMIT()) return past_limit(jitter);
#if 1
# define past_limit(j) 0
#else
static int past_limit(mz_jit_state *jitter)
{
  if (((uintptr_t)jit_get_ip().ptr > (uintptr_t)jitter->limit + JIT_BUFFER_PAD_SIZE)
      || (jitter->retain_start)) {
    printf("way past\n"); abort();
  }
  return 0;
}
#endif

void *scheme_generate_one(mz_jit_state *old_jitter, 
			  Generate_Proc generate,
			  void *data,
			  int gcable,
			  void *save_ptr,
			  Scheme_Native_Closure_Data *ndata);
int scheme_mz_is_closure(mz_jit_state *jitter, int i, int arity, int *_flags);
void scheme_mz_runstack_saved(mz_jit_state *jitter);
int scheme_mz_runstack_restored(mz_jit_state *jitter);
void scheme_mz_flostack_restore(mz_jit_state *jitter, int space, int pos, int gen, int adj);
int scheme_mz_flostack_save(mz_jit_state *jitter, int *pos);
int scheme_mz_compute_runstack_restored(mz_jit_state *jitter, int adj, int skip);
int scheme_mz_retain_it(mz_jit_state *jitter, void *v);
double *scheme_mz_retain_double(mz_jit_state *jitter, double d);
int scheme_mz_remap_it(mz_jit_state *jitter, int i);
void scheme_mz_pushr_p_it(mz_jit_state *jitter, int reg);
void scheme_mz_popr_p_it(mz_jit_state *jitter, int reg, int discard);
void scheme_mz_need_space(mz_jit_state *jitter, int need_extra);
int scheme_stack_safety(mz_jit_state *jitter, int cnt, int offset);
#ifdef USE_FLONUM_UNBOXING
int scheme_mz_flonum_pos(mz_jit_state *jitter, int i);
#endif
void scheme_mz_load_retained(mz_jit_state *jitter, int rs, void *o);

void scheme_mz_runstack_skipped(mz_jit_state *jitter, int n);
void scheme_mz_runstack_unskipped(mz_jit_state *jitter, int n);
void scheme_mz_runstack_pushed(mz_jit_state *jitter, int n);
void scheme_mz_runstack_closure_pushed(mz_jit_state *jitter, int a, int flags);
void scheme_mz_runstack_flonum_pushed(mz_jit_state *jitter, int pos);
void scheme_mz_runstack_popped(mz_jit_state *jitter, int n);
int scheme_mz_try_runstack_pop(mz_jit_state *jitter, int n);

#define mz_runstack_skipped(j, n) scheme_mz_runstack_skipped(j, n) 
#define mz_runstack_unskipped(j, n) scheme_mz_runstack_unskipped(j, n) 
#define mz_runstack_pushed(j, n) scheme_mz_runstack_pushed(j, n) 
#define mz_runstack_closure_pushed(j, n, f) scheme_mz_runstack_closure_pushed(j, n, f) 
#define mz_runstack_flonum_pushed(j, n) scheme_mz_runstack_flonum_pushed(j, n) 
#define mz_runstack_popped(j, n) scheme_mz_runstack_popped(j, n) 
#define mz_try_runstack_pop(j, n) scheme_mz_try_runstack_pop(j, n) 

/**********************************************************************/
/*                             jitinline                              */
/**********************************************************************/

int scheme_inlined_unary_prim(Scheme_Object *o, Scheme_Object *_app, mz_jit_state *jitter);
int scheme_inlined_binary_prim(Scheme_Object *o, Scheme_Object *_app, mz_jit_state *jitter);
int scheme_inlined_nary_prim(Scheme_Object *o, Scheme_Object *_app);
int scheme_generate_inlined_unary(mz_jit_state *jitter, Scheme_App2_Rec *app, int is_tail, int multi_ok, 
				  Branch_Info *for_branch, int branch_short, int need_sync, int result_ignored);
int scheme_generate_inlined_binary(mz_jit_state *jitter, Scheme_App3_Rec *app, int is_tail, int multi_ok, 
				   Branch_Info *for_branch, int branch_short, int need_sync, int result_ignored);
int scheme_generate_inlined_nary(mz_jit_state *jitter, Scheme_App_Rec *app, int is_tail, int multi_ok, 
                                 Branch_Info *for_branch, int branch_short, int result_ignored);
int scheme_generate_inlined_test(mz_jit_state *jitter, Scheme_Object *obj, int branch_short, 
                                 Branch_Info *for_branch, int need_sync);
int scheme_generate_cons_alloc(mz_jit_state *jitter, int rev, int inline_retry);

/**********************************************************************/
/*                             jitalloc                               */
/**********************************************************************/

#ifdef CAN_INLINE_ALLOC
int scheme_inline_alloc(mz_jit_state *jitter, int amt, Scheme_Type ty, int immut,
			int keep_r0_r1, int keep_fpr1, int inline_retry);
int scheme_generate_alloc_retry(mz_jit_state *jitter, int i);
#else
Scheme_Object *scheme_jit_make_list(GC_CAN_IGNORE Scheme_Object **rs, intptr_t n);
Scheme_Object *scheme_jit_make_list_star(GC_CAN_IGNORE Scheme_Object **rs, intptr_t n);
Scheme_Object *scheme_jit_make_vector(intptr_t n);
Scheme_Object *scheme_jit_make_one_element_vector(Scheme_Object *a);
Scheme_Object *scheme_jit_make_two_element_vector(Scheme_Object *a, Scheme_Object *b);
Scheme_Object *scheme_jit_make_ivector(intptr_t n);
Scheme_Object *scheme_jit_make_one_element_ivector(Scheme_Object *a);
Scheme_Object *scheme_jit_make_two_element_ivector(Scheme_Object *a, Scheme_Object *b);
#endif

/**********************************************************************/
/*                             jitarith                               */
/**********************************************************************/

int scheme_can_unbox_inline(Scheme_Object *obj, int fuel, int regs, int unsafely);
int scheme_can_unbox_directly(Scheme_Object *obj);
int scheme_generate_unboxing(mz_jit_state *jitter, int target);
int scheme_generate_pop_unboxed(mz_jit_state *jitter);
int scheme_generate_nary_arith(mz_jit_state *jitter, Scheme_App_Rec *app,
                               int arith, int cmp, Branch_Info *for_branch, int branch_short);
int scheme_generate_alloc_double(mz_jit_state *jitter, int inline_retry);
int scheme_generate_arith(mz_jit_state *jitter, Scheme_Object *rator, Scheme_Object *rand, Scheme_Object *rand2, 
			  int orig_args, int arith, int cmp, int v, 
                          Branch_Info *for_branch, int branch_short,
                          int unsafe_fx, int unsafe_fl, GC_CAN_IGNORE jit_insn *overflow_refslow);

/**********************************************************************/
/*                              jitcall                               */
/**********************************************************************/

typedef struct jit_direct_arg jit_direct_arg;

void *scheme_generate_shared_call(int num_rands, mz_jit_state *old_jitter, int multi_ok, int is_tail, 
				  int direct_prim, int direct_native, int nontail_self);
void scheme_ensure_retry_available(mz_jit_state *jitter, int multi_ok);
int scheme_generate_app(Scheme_App_Rec *app, Scheme_Object **alt_rands, int num_rands, 
			mz_jit_state *jitter, int is_tail, int multi_ok, int no_call);
int scheme_generate_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs, 
                              int is_inline, Scheme_Native_Closure *direct_to_code, jit_direct_arg *direct_arg);
int scheme_generate_non_tail_call(mz_jit_state *jitter, int num_rands, int direct_native, int need_set_rs, 
				  int multi_ok, int nontail_self, int pop_and_jump, int is_inlined);
int scheme_generate_finish_tail_call(mz_jit_state *jitter, int direct_native);
void scheme_jit_register_sub_func(mz_jit_state *jitter, void *code, Scheme_Object *protocol);
void scheme_jit_register_helper_func(mz_jit_state *jitter, void *code);
#ifdef MZ_USE_FUTURES
Scheme_Object *scheme_noncm_prim_indirect(Scheme_Prim proc, int argc);
Scheme_Object *scheme_prim_indirect(Scheme_Primitive_Closure_Proc proc, int argc, Scheme_Object *self);
#endif

/**********************************************************************/
/*                             jitstack                               */
/**********************************************************************/

void scheme_jit_add_symbol(uintptr_t start, uintptr_t end, void *value, int gc_able);
void *scheme_decrement_cache_stack_pos(void *p);
void scheme_register_stack_cache_stack(void);
#ifdef MZ_PRECISE_GC
void scheme_jit_release_native_code(void *fnlized, void *p);
#endif

/**********************************************************************/
/*                            jitcommon                               */
/**********************************************************************/

int scheme_do_generate_common(mz_jit_state *jitter, void *_data);
int scheme_do_generate_more_common(mz_jit_state *jitter, void *_data);

/**********************************************************************/
/*                               jit                                  */
/**********************************************************************/

int scheme_generate_non_tail(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int need_ends, int ignored);
int scheme_generate_non_tail_with_branch(Scheme_Object *obj, mz_jit_state *jitter, int multi_ok, int need_ends, int ignored,  
                                         Branch_Info *for_branch);
int scheme_generate(Scheme_Object *obj, mz_jit_state *jitter, int tail_ok, int wcm_may_replace, int multi_ok, int target,
                    Branch_Info *for_branch);
int scheme_generate_unboxed(Scheme_Object *obj, mz_jit_state *jitter, int inlined_ok, int unbox_anyway);
void *scheme_generate_shared_call(int num_rands, mz_jit_state *old_jitter, int multi_ok, int is_tail, 
				  int direct_prim, int direct_native, int nontail_self);

#ifdef USE_FLONUM_UNBOXING
int scheme_generate_flonum_local_unboxing(mz_jit_state *jitter, int push);
#endif
int scheme_generate_unboxed(Scheme_Object *obj, mz_jit_state *jitter, int inlined_ok, int unbox_anyway);
int scheme_generate_non_tail_mark_pos_prefix(mz_jit_state *jitter);
void scheme_generate_non_tail_mark_pos_suffix(mz_jit_state *jitter);

Scheme_Object **scheme_on_demand(Scheme_Object **argv);
Scheme_Object **scheme_on_demand_with_args(Scheme_Object **in_argv, Scheme_Object **argv, int argv_delta);

void scheme_jit_allocate_values(int count, Scheme_Thread *p);

void scheme_prepare_branch_jump(mz_jit_state *jitter, Branch_Info *for_branch);
void scheme_branch_for_true(mz_jit_state *jitter, Branch_Info *for_branch);
void scheme_add_or_patch_branch_true_uc(mz_jit_state *jitter, Branch_Info *for_branch, jit_insn *ref);
void scheme_add_or_patch_branch_true_movi(mz_jit_state *jitter, Branch_Info *for_branch, jit_insn *ref);
void scheme_add_branch_false(Branch_Info *for_branch, jit_insn *ref);
void scheme_add_branch_false_movi(Branch_Info *for_branch, jit_insn *ref);

int scheme_ok_to_move_local(Scheme_Object *obj);
int scheme_ok_to_delay_local(Scheme_Object *obj);
int scheme_can_delay_and_avoids_r1(Scheme_Object *obj);
int scheme_can_delay_and_avoids_r1_r2(Scheme_Object *obj);
int scheme_is_constant_and_avoids_r1(Scheme_Object *obj);
int scheme_is_relatively_constant_and_avoids_r1_maybe_fp(Scheme_Object *obj, Scheme_Object *wrt,
                                                         int fp_ok);
int scheme_is_relatively_constant_and_avoids_r1(Scheme_Object *obj, Scheme_Object *wrt);
int scheme_needs_only_target_register(Scheme_Object *obj, int and_can_reorder);
int scheme_is_noncm(Scheme_Object *a, mz_jit_state *jitter, int depth, int stack_start);
int scheme_is_simple(Scheme_Object *obj, int depth, int just_markless, mz_jit_state *jitter, int stack_start);
#define INIT_SIMPLE_DEPTH 10
int scheme_is_non_gc(Scheme_Object *obj, int depth);

#ifdef USE_FLONUM_UNBOXING
int scheme_jit_check_closure_flonum_bit(Scheme_Closure_Data *data, int pos, int delta);
# define CLOSURE_ARGUMENT_IS_FLONUM(data, pos) scheme_jit_check_closure_flonum_bit(data, pos, 0)
# define CLOSURE_CONTENT_IS_FLONUM(data, pos) scheme_jit_check_closure_flonum_bit(data, pos, data->num_params)
#endif

Scheme_Object *scheme_extract_global(Scheme_Object *o, Scheme_Native_Closure *nc);
Scheme_Object *scheme_extract_closure_local(Scheme_Object *obj, mz_jit_state *jitter, int extra_push);

void scheme_jit_register_traversers(void);
#ifdef MZ_USE_LWC
Scheme_Object *scheme_jit_continuation_apply_install(Apply_LWC_Args *args);
#endif


/**********************************************************************/

/* Arithmetic operation codes. Used in jitarith.c and jitinline.c. */

/*  +, add1, fx+, unsafe-fx+, fl+, unsafe-fl+ */
#define ARITH_ADD      1
/*  -, sub1, fx-, unsafe-fx-, fl-, unsafe-fl- */
#define ARITH_SUB     -1
/*  *, fx*, unsafe-fx*, fl*, unsafe-fl* */
#define ARITH_MUL      2
/*  /, fl/, unsafe-fl/ */
#define ARITH_DIV     -2
/*  quotient, fxquotient, unsafe-fxquotient */
#define ARITH_QUOT    -3
/*  remainder, fxremainder, unsafe-fxremainder */
#define ARITH_REM     -4
/*  modulo, fxmodulo, unsafe-fxmodulo */
#define ARITH_MOD     -5
/*  bitwise-and, fxand, unsafe-fxand */
#define ARITH_AND      3
/*  bitwise-ior, fxior, unsafe-fxior */
#define ARITH_IOR      4
/*  bitwise-xor, fxxor, unsafe-fxxor */
#define ARITH_XOR      5
/*  arithmetic-shift, fxlshift, unsafe-fxlshift */
#define ARITH_LSH      6
/*  fxrshift, unsafe-fxrshift */
#define ARITH_RSH     -6
/*  bitwise-not, fxnot, unsafe-fxnot */
#define ARITH_NOT      7
/*  min, fxmin, unsafe-fxmin, flmin, unsafe-flmin */
#define ARITH_MIN      9
/*  max, fxmax, unsafe-fxmax, flmax, unsafe-flmax */
#define ARITH_MAX      10
/*  abs, fxabs, unsafe-fxabs, flabs, unsafe-flabs */
#define ARITH_ABS      11
/*  exact->inexact, real->double-flonum, unsafe-fx->fl, ->fl, fx->fl */
#define ARITH_EX_INEX  12
/*  sqrt, flsqrt, unsafe-flsqrt */
#define ARITH_SQRT     13
/*  flfloor, flceiling, flround, fltruncate, flsin,  flcos, fltan, */
/*  flasin, flacos, flatan, flexp, fllog */
#define ARITH_FLUNOP   14
/*  inexact->exact, unsafe-fl->fx, fl->exact-integer, fl->fx */
#define ARITH_INEX_EX  15
/*  flexpt */
#define ARITH_EXPT     16

/* Comparison codes. Used in jitarith.c and jitinline.c. */

/*  zero?, =, fx=, unsafe-fx=, fl=, unsafe-fl= */
#define CMP_EQUAL  0
/*  >=, fx>=, unsafe-fx>=, fl>=, unsafe-fl>= */
#define CMP_GEQ    1
/*  <=, fx<=, unsafe-fx<=, fl<=, unsafe-fl<= */
#define CMP_LEQ   -1
/*  >, fx>, unsafe-fx>, fl>, unsafe-fl>, positive? */
#define CMP_GT     2
/*  <, fx<, unsafe-fx<, fl<, unsafe-fl<, negative? */
#define CMP_LT    -2
/*  bitwise-bit-test? */
#define CMP_BIT    3
/*  even? */
#define CMP_EVENP  4
/*  odd? */
#define CMP_ODDP  -4
