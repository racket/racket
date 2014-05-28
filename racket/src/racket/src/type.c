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

#include "schpriv.h"
#include <string.h>

/* types should all be registered before invoking places */

SHARED_OK Scheme_Type_Reader *scheme_type_readers;
SHARED_OK Scheme_Type_Writer *scheme_type_writers;
SHARED_OK Scheme_Equal_Proc *scheme_type_equals;
SHARED_OK Scheme_Primary_Hash_Proc *scheme_type_hash1s;
SHARED_OK Scheme_Secondary_Hash_Proc *scheme_type_hash2s;

SHARED_OK static char **type_names;
SHARED_OK static Scheme_Type maxtype, allocmax;

#ifdef MEMORY_COUNTING_ON
SHARED_OK intptr_t scheme_type_table_count;
#endif

#ifdef MZ_USE_PLACES
static mzrt_mutex *type_array_mutex;
#endif

#define RAW_MALLOC_N(t, n) (t*)malloc(n * sizeof(t))

static void init_type_arrays()
{
  intptr_t n;

#ifdef MZ_USE_PLACES
  mzrt_mutex_create(&type_array_mutex);
#endif

  REGISTER_SO(type_names);
  REGISTER_SO(scheme_type_readers);
  REGISTER_SO(scheme_type_writers);
  REGISTER_SO(scheme_type_equals);
  REGISTER_SO(scheme_type_hash1s);
  REGISTER_SO(scheme_type_hash2s);
  
  maxtype = _scheme_last_type_;
  allocmax = maxtype + 100;

  type_names = RAW_MALLOC_N(char *, allocmax);
  memset(type_names, 0, allocmax * sizeof(char *));
  scheme_type_readers = RAW_MALLOC_N(Scheme_Type_Reader, allocmax);
  n = allocmax * sizeof(Scheme_Type_Reader);
  memset(scheme_type_readers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
  scheme_misc_count += (allocmax * sizeof(char *));
#endif

  scheme_type_writers = RAW_MALLOC_N(Scheme_Type_Writer, allocmax);
  n = allocmax * sizeof(Scheme_Type_Writer);
  memset(scheme_type_writers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
#endif  

  scheme_type_equals = RAW_MALLOC_N(Scheme_Equal_Proc, allocmax);
  n = allocmax * sizeof(Scheme_Equal_Proc);
  memset(scheme_type_equals, 0, n);

  scheme_type_hash1s = RAW_MALLOC_N(Scheme_Primary_Hash_Proc, allocmax);
  n = allocmax * sizeof(Scheme_Primary_Hash_Proc);
  memset(scheme_type_hash1s, 0, n);

  scheme_type_hash2s = RAW_MALLOC_N(Scheme_Secondary_Hash_Proc, allocmax);
  n = allocmax * sizeof(Scheme_Secondary_Hash_Proc);
  memset(scheme_type_hash2s, 0, n);
}

void
scheme_init_type ()
{
  if (!type_names)
    init_type_arrays();

#define set_name(t, n) type_names[t] = n

  set_name(scheme_true_type, "<true>");
  set_name(scheme_false_type, "<false>");
  set_name(scheme_char_type, "<char>");
  set_name(scheme_local_type, "<local-code>");
  set_name(scheme_local_unbox_type, "<local-unbox-code>");
  set_name(scheme_variable_type, "<global-variable-code>");
  set_name(scheme_toplevel_type, "<variable-code>");
  set_name(scheme_module_variable_type, "<module-variable-code>");
  set_name(scheme_application_type, "<application-code>");
  set_name(scheme_application2_type, "<unary-application-code>");
  set_name(scheme_application3_type, "<binary-application-code>");
  set_name(scheme_compiled_unclosed_procedure_type, "<procedure-semi-code>");
  set_name(scheme_unclosed_procedure_type, "<procedure-code>");
  set_name(scheme_branch_type, "<branch-code>");
  set_name(scheme_sequence_type, "<sequence-code>");
  set_name(scheme_with_cont_mark_type, "<with-continuation-mark-code>");
  set_name(scheme_quote_syntax_type, "<quote-syntax-code>");

  set_name(scheme_define_values_type, "<define-values-code>");
  set_name(scheme_define_syntaxes_type, "<define-syntaxes-code>");
  set_name(scheme_begin_for_syntax_type, "<begin-for-syntax-code>");
  set_name(scheme_begin0_sequence_type, "<begin0-code>");
  set_name(scheme_splice_sequence_type, "<splicing-begin-code>");
  set_name(scheme_module_type, "<module-code>");
  set_name(scheme_set_bang_type, "<set!-code>");
  set_name(scheme_boxenv_type, "<boxenv-code>");
  set_name(scheme_require_form_type, "<require-code>");
  set_name(scheme_varref_form_type, "<varref-code>");
  set_name(scheme_apply_values_type, "<apply-values-code>");
  set_name(scheme_case_lambda_sequence_type, "<case-lambda-code>");

  set_name(scheme_let_value_type, "<let-value-code>");
  set_name(scheme_let_void_type, "<let-void-code>");
  set_name(scheme_compiled_let_value_type, "<let-value-semi-code>");
  set_name(scheme_compiled_let_void_type, "<let-void-semi-code>");
  set_name(scheme_compiled_toplevel_type, "<variable-semi-code>");
  set_name(scheme_compiled_quote_syntax_type, "<quote-syntax-semi-code>");
  set_name(scheme_letrec_type, "<letrec-code>");
  set_name(scheme_let_one_type, "<let-one-code>");
  set_name(scheme_quote_compilation_type, "<quote-code>");

  set_name(scheme_eval_waiting_type, "<eval-waiting>");
  set_name(scheme_void_type, "<void>");
  set_name(scheme_prim_type, "<primitive>");
  set_name(scheme_closed_prim_type, "<primitive-closure>");
  set_name(scheme_closure_type, "<procedure>");
  set_name(scheme_native_closure_type, "<procedure>");
  set_name(scheme_cont_type, "<continuation>");
  set_name(scheme_tail_call_waiting_type, "<tail-call-waiting>");
  set_name(scheme_null_type, "<empty-list>");
  set_name(scheme_pair_type, "<pair>");
  set_name(scheme_mutable_pair_type, "<mutable-pair>");
  set_name(scheme_raw_pair_type, "<raw-pair>");
  set_name(scheme_box_type, "<box>");
  set_name(scheme_integer_type, "<fixnum-integer>");
  set_name(scheme_double_type, "<inexact-number>");
  set_name(scheme_long_double_type, "<extflonum>");
  set_name(scheme_float_type, "<inexact-number*>");
  set_name(scheme_undefined_type, "<unsafe-undefined>");
  set_name(scheme_eof_type, "<eof>");
  set_name(scheme_input_port_type, "<input-port>");
  set_name(scheme_output_port_type, "<output-port>");
  set_name(scheme_thread_type, "<thread>");
  set_name(scheme_char_string_type, "<string>");
  set_name(scheme_byte_string_type, "<byte-string>");
  set_name(scheme_unix_path_type, "<unix-path>");
  set_name(scheme_windows_path_type, "<windows-path>");
  set_name(scheme_struct_property_type, "<struct-property>");
  set_name(scheme_chaperone_property_type, "<chaperone-property>");
  set_name(scheme_structure_type, "<struct>");
  set_name(scheme_proc_chaperone_type, "<chaperone>");
  set_name(scheme_chaperone_type, "<chaperone>");
#ifdef USE_SENORA_GC
  set_name(scheme_proc_struct_type, "<procedure-struct>");
#else
  set_name(scheme_proc_struct_type, "<struct>");
#endif
  set_name(scheme_symbol_type, "<symbol>");
  set_name(scheme_keyword_type, "<keyword>");
  set_name(scheme_syntax_compiler_type, "<syntax-compiler>");
  set_name(scheme_macro_type, "<macro>");
  set_name(scheme_vector_type, "<vector>");
  set_name(scheme_flvector_type, "<flvector>");
  set_name(scheme_extflvector_type, "<extflvector>");
  set_name(scheme_fxvector_type, "<fxvector>");
  set_name(scheme_bignum_type, "<bignum-integer>");
  set_name(scheme_escaping_cont_type, "<escape-continuation>");
  set_name(scheme_sema_type, "<semaphore>");
  set_name(scheme_channel_type, "<channel>");
  set_name(scheme_channel_put_type, "<channel-put>");
  set_name(scheme_hash_table_type, "<hash>");
  set_name(scheme_hash_tree_type, "<hash>");
  set_name(scheme_bucket_table_type, "<hash>");
  set_name(scheme_module_registry_type, "<module-registry>");
  set_name(scheme_case_closure_type, "<procedure>");
  set_name(scheme_placeholder_type, "<placeholder>");
  set_name(scheme_table_placeholder_type, "<hash-table-placeholder>");
  set_name(scheme_weak_box_type, "<weak-box>");
  set_name(scheme_ephemeron_type, "<ephemeron>");
  set_name(scheme_rational_type, "<fractional-number>");
  set_name(scheme_complex_type, "<complex-number>");
  set_name(scheme_struct_type_type, "<struct-type>");
  set_name(scheme_listener_type, "<tcp-listener>");
  set_name(scheme_tcp_accept_evt_type, "<tcp-accept-evt>");
  set_name(scheme_filesystem_change_evt_type, "<filesystem-change-evt>");
  set_name(scheme_namespace_type, "<namespace>");
  set_name(scheme_config_type, "<parameterization>");
  set_name(scheme_will_executor_type, "<will-executor>");
  set_name(scheme_random_state_type, "<pseudo-random-generator>");
  set_name(scheme_regexp_type, "<regexp>");
  set_name(scheme_rename_table_type, "<rename-table>");
  set_name(scheme_bucket_type, "<hash-table-bucket>");
  set_name(scheme_prefix_type, "<runtime-prefix>");
  set_name(scheme_resolve_prefix_type, "<resolve-prefix>");
  set_name(scheme_readtable_type, "<readtable>");

  set_name(scheme_compilation_top_type, "<compiled-code>");

  set_name(scheme_svector_type, "<short-vector>");

  set_name(scheme_custodian_type, "<custodian>");
  set_name(scheme_cust_box_type, "<custodian-box>");
  set_name(scheme_plumber_type, "<plumber>");
  set_name(scheme_plumber_handle_type, "<plumber-flush-handle>");
  set_name(scheme_cont_mark_set_type, "<continuation-mark-set>");
  set_name(scheme_cont_mark_chain_type, "<chain>");

  set_name(scheme_inspector_type, "<inspector>");
  
  set_name(scheme_stx_type, "<syntax>");
  set_name(scheme_stx_offset_type, "<internal-syntax-offset>");
  set_name(scheme_expanded_syntax_type, "<expanded-syntax>");
  set_name(scheme_set_macro_type, "<set!-transformer>");
  set_name(scheme_id_macro_type, "<rename-transformer>");

  set_name(scheme_module_index_type, "<module-path-index>");

  set_name(scheme_subprocess_type, "<subprocess>");

  set_name(scheme_cpointer_type, "<cpointer>");

  set_name(scheme_wrap_chunk_type, "<wrap-chunk>");

  set_name(scheme_security_guard_type, "<security-guard>");

  set_name(scheme_indent_type, "<internal-indentation>");

  set_name(scheme_udp_type, "<udp-socket>");
  set_name(scheme_udp_evt_type, "<udp-socket-evt>");

  set_name(scheme_evt_set_type, "<evt-set>");
  set_name(scheme_wrap_evt_type, "<evt>");
  set_name(scheme_handle_evt_type, "<evt>");
  set_name(scheme_nack_evt_type, "<evt>");
  set_name(scheme_nack_guard_evt_type, "<evt>");
  set_name(scheme_poll_evt_type, "<evt>");
  set_name(scheme_semaphore_repost_type, "<semaphore-peek>");
  set_name(scheme_alarm_type, "<alarm-evt>");
  set_name(scheme_progress_evt_type, "<progress-evt>");
  set_name(scheme_write_evt_type, "<write-evt>");
  set_name(scheme_always_evt_type, "<always-evt>");
  set_name(scheme_never_evt_type, "<never-evt>");
  set_name(scheme_thread_recv_evt_type, "<thread-receive-evt>");
  set_name(scheme_port_closed_evt_type, "<port-closed-evt>");

  set_name(scheme_thread_resume_type, "<thread-resume-evt>");
  set_name(scheme_thread_suspend_type, "<thread-suspend-evt>");
  set_name(scheme_thread_dead_type, "<thread-dead-evt>");

  set_name(scheme_thread_set_type, "<thread-set>");
  set_name(scheme_thread_cell_type, "<thread-cell>");
  set_name(scheme_thread_cell_values_type, "<thread-cell-values>");

  set_name(scheme_prompt_tag_type, "<continuation-prompt-tag>");
  set_name(scheme_continuation_mark_key_type, "<continuation-mark-key>");

  set_name(scheme_string_converter_type, "<string-converter>");

  set_name(scheme_channel_syncer_type, "<channel-syncer>");

  set_name(scheme_special_comment_type, "<special-comment>");

  set_name(scheme_global_ref_type, "<variable-reference>");

  set_name(scheme_delay_syntax_type, "<on-demand-stub>");

  set_name(scheme_intdef_context_type, "<internal-definition-context>");
  set_name(scheme_lexical_rib_type, "<internal:lexical-rib>");

  set_name(scheme_already_comp_type, "<internal:already-compiled>");

  set_name(scheme_logger_type, "<logger>");
  set_name(scheme_log_reader_type, "<log-receiver>");

  set_name(scheme_future_type, "<future>");
  set_name(scheme_fsemaphore_type, "<fsemaphore>");

  set_name(_scheme_values_types_, "<resurrected>");
  set_name(_scheme_compiled_values_types_, "<internal>");

  set_name(scheme_place_type, "<place>");
  set_name(scheme_place_async_channel_type, "<place-half-channel>");
  set_name(scheme_place_bi_channel_type, "<place-channel>");
  set_name(scheme_place_dead_type, "<place-dead-evt>");

  set_name(scheme_resolved_module_path_type, "<resolve-module-path>");

  set_name(scheme_phantom_bytes_type, "<phantom-bytes>");

  set_name(scheme_environment_variables_type, "<environment-variables>");

#ifdef MZ_GC_BACKTRACE
  set_name(scheme_rt_meta_cont, "<meta-continuation>");
#endif
}

Scheme_Type scheme_make_type(const char *name)
{
  Scheme_Type newtype;

  if (!type_names)
    init_type_arrays();

#ifdef MZ_USE_PLACES
  mzrt_mutex_lock(type_array_mutex);
#endif

  if (maxtype == allocmax) {
    /* Expand arrays */
    void *naya;
    intptr_t n;
    
    allocmax += 20;

    naya = malloc(allocmax * sizeof(char *));
    memcpy(naya, type_names, maxtype * sizeof(char *));
    memset(naya, 0, maxtype * sizeof(char *));
    free(type_names);
    type_names = (char **)naya;

    naya = malloc(n = allocmax * sizeof(Scheme_Type_Reader));
    memset(naya, 0, n);
    memcpy(naya, scheme_type_readers, maxtype * sizeof(Scheme_Type_Reader));
    free(scheme_type_readers);
    scheme_type_readers = (Scheme_Type_Reader *)naya;

    naya = malloc(n = allocmax * sizeof(Scheme_Type_Writer));
    memset(naya, 0, n);
    memcpy(naya, scheme_type_writers, maxtype * sizeof(Scheme_Type_Writer));
    free(scheme_type_writers);
    scheme_type_writers = (Scheme_Type_Writer *)naya;

    naya = malloc(n = allocmax * sizeof(Scheme_Equal_Proc));
    memset(naya, 0, n);
    memcpy(naya, scheme_type_equals, maxtype * sizeof(Scheme_Equal_Proc));
    free(scheme_type_equals);
    scheme_type_equals = (Scheme_Equal_Proc *)naya;

    naya = malloc(n = allocmax * sizeof(Scheme_Primary_Hash_Proc));
    memset(naya, 0, n);
    memcpy(naya, scheme_type_hash1s, maxtype * sizeof(Scheme_Primary_Hash_Proc));
    free(scheme_type_hash1s);
    scheme_type_hash1s = (Scheme_Primary_Hash_Proc *)naya;

    naya = malloc(n = allocmax * sizeof(Scheme_Secondary_Hash_Proc));
    memset(naya, 0, n);
    memcpy(naya, scheme_type_hash2s, maxtype * sizeof(Scheme_Secondary_Hash_Proc));
    free(scheme_type_hash2s);
    scheme_type_hash2s = (Scheme_Secondary_Hash_Proc *)naya;

#ifdef MEMORY_COUNTING_ON
    scheme_type_table_count += 20 * (sizeof(Scheme_Type_Reader)
                                     + sizeof(Scheme_Type_Writer));
    scheme_misc_count += (20 * sizeof(char *));
#endif
  }

  {
    char *tn;
    int len;
    len = strlen(name) + 1;
    tn = (char *)malloc(len);
    memcpy(tn, name, len);
    type_names[maxtype] = tn;
  }

  newtype = maxtype;
  maxtype++;

#ifdef MZ_USE_PLACES
  mzrt_mutex_unlock(type_array_mutex);
#endif

  return newtype;
}

char *scheme_get_type_name_or_null(Scheme_Type t)
{
  if (t < 0 || t >= maxtype)
    return "<bad-value>";
  return type_names[t];
}

char *scheme_get_type_name(Scheme_Type t)
{
  char *s;
  s = scheme_get_type_name_or_null(t);
  return s ? s : "???";
}

void scheme_install_type_reader(Scheme_Type t, Scheme_Type_Reader f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_readers[t] = f;
}

void scheme_install_type_writer(Scheme_Type t, Scheme_Type_Writer f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_writers[t] = f;
}


void scheme_set_type_equality(Scheme_Type t, 
                              Scheme_Equal_Proc f,
                              Scheme_Primary_Hash_Proc hash1,
                              Scheme_Secondary_Hash_Proc hash2)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_equals[t] = f;
  scheme_type_hash1s[t] = hash1;
  scheme_type_hash2s[t] = hash2;
}

int scheme_num_types(void)
{
  return maxtype;
}

/***********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

static int bad_trav_SIZE(void *p, struct NewGC *gc)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

static int bad_trav_MARK(void *p, struct NewGC *gc)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

static int bad_trav_FIXUP(void *p, struct NewGC *gc)
{
  printf("Shouldn't get here.\n");
  exit(1);
}

#define bad_trav_IS_CONST_SIZE 0
#define bad_trav_IS_ATOMIC 0

static void MARK_cjs(Scheme_Continuation_Jump_State *cjs, struct NewGC *gc)
{
  gcMARK2(cjs->jumping_to_continuation, gc);
  gcMARK2(cjs->alt_full_continuation, gc);
  gcMARK2(cjs->val, gc);
}

static void FIXUP_cjs(Scheme_Continuation_Jump_State *cjs, struct NewGC *gc)
{
  gcFIXUP2(cjs->jumping_to_continuation, gc);
  gcFIXUP2(cjs->alt_full_continuation, gc);
  gcFIXUP2(cjs->val, gc);
}

static void MARK_stack_state(Scheme_Stack_State *ss, struct NewGC *gc)
{
}

static void FIXUP_stack_state(Scheme_Stack_State *ss, struct NewGC *gc)
{
}

static void MARK_jmpup(Scheme_Jumpup_Buf *buf, struct NewGC *gc)
{
  gcMARK2(buf->stack_copy, gc);
  gcMARK2(buf->cont, gc);
  gcMARK2(buf->external_stack, gc);

  /* IMPORTANT: the buf->stack_copy pointer must be the only instance
     of this stack to be traversed. If you copy a jmpup buffer (as in
     fun.c), don't let a GC happen until the old copy is zeroed
     out. */
  if (buf->stack_copy)
    GC_mark2_variable_stack(buf->gc_var_stack,
                            (intptr_t)buf->stack_copy - (intptr_t)buf->stack_from,
                            /* FIXME: stack direction */
                            (char *)buf->stack_copy + buf->stack_size,
                            buf->stack_copy,
                            gc);
}

static void FIXUP_jmpup(Scheme_Jumpup_Buf *buf, struct NewGC *gc)
{
  void *new_stack;

  new_stack = GC_resolve(buf->stack_copy);
  gcFIXUP2_TYPED_NOW(void *, buf->stack_copy, gc);
  gcFIXUP2(buf->cont, gc);
  gcFIXUP2(buf->external_stack, gc);

  if (buf->stack_copy)
    GC_fixup2_variable_stack(buf->gc_var_stack,
                             (intptr_t)new_stack - (intptr_t)buf->stack_from,
                             /* FIXME: stack direction */
                             (char *)new_stack + buf->stack_size,
                             new_stack,
                             gc);
}

#define RUNSTACK_ZERO_VAL NULL

#include "mzmark_type.inc"

void scheme_register_traversers(void)
{
  GC_REG_TRAV(scheme_toplevel_type, toplevel_obj);
  GC_REG_TRAV(scheme_variable_type, variable_obj);
  GC_REG_TRAV(scheme_local_type, local_obj);
  GC_REG_TRAV(scheme_local_unbox_type, local_obj);
  GC_REG_TRAV(scheme_application_type, app_rec);
  GC_REG_TRAV(scheme_application2_type, app2_rec);
  GC_REG_TRAV(scheme_application3_type, app3_rec);
  GC_REG_TRAV(scheme_sequence_type, seq_rec);
  GC_REG_TRAV(scheme_branch_type, branch_rec);
  GC_REG_TRAV(scheme_unclosed_procedure_type, unclosed_proc);
  GC_REG_TRAV(scheme_let_value_type, let_value);
  GC_REG_TRAV(scheme_let_void_type, let_void);
  GC_REG_TRAV(scheme_letrec_type, letrec);
  GC_REG_TRAV(scheme_let_one_type, let_one);
  GC_REG_TRAV(scheme_with_cont_mark_type, with_cont_mark);
  GC_REG_TRAV(scheme_quote_syntax_type, quotesyntax_obj);
  GC_REG_TRAV(scheme_module_variable_type, module_var);

  GC_REG_TRAV(scheme_define_values_type, vector_obj);
  GC_REG_TRAV(scheme_define_syntaxes_type, vector_obj);
  GC_REG_TRAV(scheme_begin_for_syntax_type, vector_obj);
  GC_REG_TRAV(scheme_varref_form_type, twoptr_obj);
  GC_REG_TRAV(scheme_apply_values_type, twoptr_obj);
  GC_REG_TRAV(scheme_boxenv_type, twoptr_obj);
  GC_REG_TRAV(scheme_case_lambda_sequence_type, case_closure);
  GC_REG_TRAV(scheme_begin0_sequence_type, seq_rec);
  GC_REG_TRAV(scheme_splice_sequence_type, seq_rec);
  GC_REG_TRAV(scheme_set_bang_type, set_bang);
  GC_REG_TRAV(scheme_module_type, module_val);
  GC_REG_TRAV(scheme_rt_export_info, exp_info_val);
  GC_REG_TRAV(scheme_require_form_type, twoptr_obj);
  GC_REG_TRAV(scheme_inline_variant_type, vector_obj);

  GC_REG_TRAV(_scheme_values_types_, bad_trav);
  
  GC_REG_TRAV(scheme_compiled_unclosed_procedure_type, unclosed_proc);
  GC_REG_TRAV(scheme_compiled_let_value_type, comp_let_value);
  GC_REG_TRAV(scheme_compiled_let_void_type, let_header);
  GC_REG_TRAV(scheme_compiled_toplevel_type, toplevel_obj);
  GC_REG_TRAV(scheme_compiled_quote_syntax_type, local_obj);

  GC_REG_TRAV(scheme_quote_compilation_type, small_object);

  GC_REG_TRAV(_scheme_compiled_values_types_, bad_trav);

  GC_REG_TRAV(scheme_prefix_type, prefix_val);
  GC_REG_TRAV(scheme_resolve_prefix_type, resolve_prefix_val);
  GC_REG_TRAV(scheme_rt_comp_prefix, comp_prefix_val);

  GC_REG_TRAV(scheme_prim_type, prim_proc);
  GC_REG_TRAV(scheme_closed_prim_type, closed_prim_proc);
  GC_REG_TRAV(scheme_closure_type, scm_closure);
  GC_REG_TRAV(scheme_case_closure_type, case_closure);
  GC_REG_TRAV(scheme_cont_type, cont_proc);
  GC_REG_TRAV(scheme_rt_dyn_wind, mark_dyn_wind);
  GC_REG_TRAV(scheme_rt_overflow, mark_overflow);
  GC_REG_TRAV(scheme_rt_overflow_jmp, mark_overflow_jmp);
  GC_REG_TRAV(scheme_rt_meta_cont, meta_cont_proc);
  GC_REG_TRAV(scheme_escaping_cont_type, escaping_cont_proc);
  GC_REG_TRAV(scheme_rt_cont_jmp, cont_jmp_proc);

  GC_REG_TRAV(scheme_char_type, small_atomic_obj);
  GC_REG_TRAV(scheme_integer_type, bad_trav);
  GC_REG_TRAV(scheme_bignum_type, bignum_obj);
  GC_REG_TRAV(scheme_rational_type, rational_obj);
  GC_REG_TRAV(scheme_float_type,  float_obj);
  GC_REG_TRAV(scheme_double_type, double_obj);
  GC_REG_TRAV(scheme_long_double_type, long_double_obj);
  GC_REG_TRAV(scheme_complex_type, complex_obj);
  GC_REG_TRAV(scheme_char_string_type, string_obj);
  GC_REG_TRAV(scheme_byte_string_type, bstring_obj);
  GC_REG_TRAV(scheme_unix_path_type, bstring_obj);
  GC_REG_TRAV(scheme_windows_path_type, bstring_obj);
  GC_REG_TRAV(scheme_symbol_type, symbol_obj);
#ifdef MZ_USE_PLACES
  GC_REG_TRAV(scheme_serialized_symbol_type, bstring_obj);
  GC_REG_TRAV(scheme_serialized_keyword_type, bstring_obj);
  GC_REG_TRAV(scheme_place_dead_type, small_object);
#endif
  GC_REG_TRAV(scheme_keyword_type, symbol_obj);
  GC_REG_TRAV(scheme_null_type, small_atomic_obj);
  GC_REG_TRAV(scheme_pair_type, cons_cell);
  GC_REG_TRAV(scheme_mutable_pair_type, cons_cell);
  GC_REG_TRAV(scheme_raw_pair_type, cons_cell);
  GC_REG_TRAV(scheme_vector_type, vector_obj);
  GC_REG_TRAV(scheme_flvector_type, flvector_obj);
#ifdef MZ_LONG_DOUBLE
  GC_REG_TRAV(scheme_extflvector_type, extflvector_obj);
#endif
  GC_REG_TRAV(scheme_fxvector_type, fxvector_obj);
  GC_REG_TRAV(scheme_cpointer_type, cpointer_obj);

  GC_REG_TRAV(scheme_bucket_type, bucket_obj);

  GC_REG_TRAV(scheme_input_port_type, input_port);
  GC_REG_TRAV(scheme_output_port_type, output_port);
  GC_REG_TRAV(scheme_eof_type, small_atomic_obj);
  GC_REG_TRAV(scheme_true_type, small_atomic_obj);
  GC_REG_TRAV(scheme_false_type, small_atomic_obj);
  GC_REG_TRAV(scheme_void_type, small_atomic_obj); 
  GC_REG_TRAV(scheme_syntax_compiler_type, syntax_compiler);
  GC_REG_TRAV(scheme_macro_type, small_object);
  GC_REG_TRAV(scheme_box_type, small_object);
  GC_REG_TRAV(scheme_thread_type, thread_val);
  GC_REG_TRAV(scheme_prompt_type, prompt_val);
  GC_REG_TRAV(scheme_prompt_tag_type, cons_cell);
  GC_REG_TRAV(scheme_continuation_mark_key_type, small_object);
  GC_REG_TRAV(scheme_cont_mark_set_type, cont_mark_set_val);
  GC_REG_TRAV(scheme_sema_type, sema_val);
  GC_REG_TRAV(scheme_channel_type, channel_val);
  GC_REG_TRAV(scheme_channel_put_type, channel_put_val);
  GC_REG_TRAV(scheme_semaphore_repost_type, small_object);
  GC_REG_TRAV(scheme_thread_suspend_type, twoptr_obj);
  GC_REG_TRAV(scheme_thread_resume_type, twoptr_obj);
  GC_REG_TRAV(scheme_thread_dead_type, small_object);
  GC_REG_TRAV(scheme_hash_table_type, hash_table_val);
  GC_REG_TRAV(scheme_bucket_table_type, bucket_table_val);
  GC_REG_TRAV(scheme_module_registry_type, module_reg_val);
  GC_REG_TRAV(scheme_namespace_type, namespace_val);
  GC_REG_TRAV(scheme_random_state_type, random_state_val);
  
  GC_REG_TRAV(scheme_compilation_top_type, compilation_top_val);
  GC_REG_TRAV(scheme_intdef_context_type, twoptr_obj);

  GC_REG_TRAV(scheme_eval_waiting_type, bad_trav);
  GC_REG_TRAV(scheme_tail_call_waiting_type, bad_trav);
  GC_REG_TRAV(scheme_undefined_type, small_atomic_obj);
  GC_REG_TRAV(scheme_placeholder_type, small_object);
  GC_REG_TRAV(scheme_table_placeholder_type, iptr_obj);

  GC_REG_TRAV(scheme_svector_type, svector_val);

  GC_REG_TRAV(scheme_set_macro_type, small_object);
  GC_REG_TRAV(scheme_id_macro_type, twoptr_obj);

  GC_REG_TRAV(scheme_stx_type, stx_val);
  GC_REG_TRAV(scheme_stx_offset_type, stx_off_val);
  GC_REG_TRAV(scheme_expanded_syntax_type, twoptr_obj);
  GC_REG_TRAV(scheme_rt_module_exports, module_exports_val);
  GC_REG_TRAV(scheme_module_phase_exports_type, module_phase_exports_val);
  GC_REG_TRAV(scheme_module_index_type, modidx_val);

  GC_REG_TRAV(scheme_security_guard_type, guard_val);

  GC_REG_TRAV(scheme_nack_evt_type, twoptr_obj);
  GC_REG_TRAV(scheme_always_evt_type, small_atomic_obj);
  GC_REG_TRAV(scheme_never_evt_type, small_atomic_obj);
  GC_REG_TRAV(scheme_thread_recv_evt_type, small_atomic_obj);
  GC_REG_TRAV(scheme_port_closed_evt_type, small_object);

  GC_REG_TRAV(scheme_inspector_type, mark_inspector);

  GC_REG_TRAV(scheme_rt_buf_holder, buf_holder);
  GC_REG_TRAV(scheme_rt_pipe, mark_pipe);

  GC_REG_TRAV(scheme_tcp_accept_evt_type, twoptr_obj);

  GC_REG_TRAV(scheme_special_comment_type, small_object);

  GC_REG_TRAV(scheme_progress_evt_type, twoptr_obj);

  GC_REG_TRAV(scheme_already_comp_type, iptr_obj);
  
  GC_REG_TRAV(scheme_thread_cell_values_type, small_object);

  GC_REG_TRAV(scheme_global_ref_type, twoptr_obj);

  GC_REG_TRAV(scheme_delay_syntax_type, small_object);

  GC_REG_TRAV(scheme_resolved_module_path_type, small_object);

  GC_REG_TRAV(scheme_logger_type, mark_logger);
  GC_REG_TRAV(scheme_log_reader_type, mark_log_reader);

  GC_REG_TRAV(scheme_rt_runstack, runstack_val);

  GC_REG_TRAV(scheme_free_id_info_type, vector_obj);

  GC_REG_TRAV(scheme_rib_delimiter_type, small_object);
  GC_REG_TRAV(scheme_noninline_proc_type, small_object);
  GC_REG_TRAV(scheme_prune_context_type, small_object);

  GC_REG_TRAV(scheme_proc_shape_type, small_object);
  GC_REG_TRAV(scheme_struct_proc_shape_type, small_atomic_obj);

  GC_REG_TRAV(scheme_environment_variables_type, small_object);

  GC_REG_TRAV(scheme_plumber_handle_type, twoptr_obj);
}

END_XFORM_SKIP;

#endif
