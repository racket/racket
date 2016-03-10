
enum {

  /* Compiled bytecode elements: */
  scheme_toplevel_type,                 /* 0 */
  scheme_local_type,                    /* 1 */
  scheme_local_unbox_type,              /* 2 */
  scheme_application_type,              /* 3 */
  scheme_application2_type,             /* 4 */
  scheme_application3_type,             /* 5 */
  scheme_sequence_type,                 /* 6 */
  scheme_branch_type,                   /* 7 */
  scheme_lambda_type,                   /* 8 */
  scheme_let_value_type,                /* 9 */
  scheme_let_void_type,                 /* 10 */
  scheme_letrec_type,                   /* 11 */
  scheme_let_one_type,                  /* 12 */
  scheme_with_cont_mark_type,           /* 13 */
  scheme_quote_syntax_type,             /* 14 */

  scheme_define_values_type,            /* 15 */
  scheme_define_syntaxes_type,          /* 16 */
  scheme_begin_for_syntax_type,         /* 17 */
  scheme_set_bang_type,                 /* 18 */
  scheme_boxenv_type,                   /* 19 */
  scheme_begin0_sequence_type,          /* 20 */
  scheme_splice_sequence_type,          /* 21 */
  scheme_require_form_type,             /* 22 */
  scheme_varref_form_type,              /* 23 */
  scheme_apply_values_type,             /* 24 */
  scheme_with_immed_mark_type,          /* 25 */
  scheme_case_lambda_sequence_type,     /* 26 */
  scheme_module_type,                   /* 27 */
  scheme_inline_variant_type,           /* 28 */

  _scheme_values_types_,                /* 29 */
  /* All following types are values at run time */
  
  /* Replacements for some of the above as the
     compiler's intermediate representation for
     optimization: */
  scheme_ir_local_type,                 /* 30 */
  scheme_ir_lambda_type,                /* 31 */
  scheme_ir_let_value_type,             /* 32 */
  scheme_ir_let_header_type,            /* 33 */
  scheme_ir_toplevel_type,              /* 34 */
  scheme_ir_quote_syntax_type,          /* 35 */

  scheme_quote_compilation_type, /* used while writing, only */

  /* Generated in the compiler front-end, but
     registered in the prefix table instead of
     used directly as an "expression": */
  scheme_variable_type,                 /* 37 */
  scheme_module_variable_type, /* link replaces with scheme_variable_type */

  _scheme_ir_values_types_,             /* 39 */
  /* All of the following are literal values from the
     perspective of the compiler */

  /* procedure types */
  scheme_prim_type,                     /* 40 */
  scheme_closed_prim_type,              /* 41 */
  scheme_closure_type,                  /* 42 */
  scheme_case_closure_type,             /* 43 */
  scheme_cont_type,                     /* 44 */
  scheme_escaping_cont_type,            /* 45 */
  scheme_proc_struct_type,              /* 46 */
  scheme_native_closure_type,           /* 47 */
  scheme_proc_chaperone_type,           /* 48 */

  scheme_chaperone_type,                /* 49 */

  /* structure type (plus one above for procs) */
  scheme_structure_type,                /* 50 */

  /* number types (must be together) */
  scheme_integer_type,                  /* 51 */
  scheme_bignum_type,                   /* 52 */
  scheme_rational_type,                 /* 53 */
  scheme_float_type,                    /* 54 */
  scheme_double_type,                   /* 55 */
  scheme_complex_type,                  /* 56 */

  /* other eqv?-able values (must be with numbers) */
  scheme_char_type,                     /* 57 */

  /* other values */
  scheme_long_double_type,              /* 58 */
  scheme_char_string_type,              /* 59 */
  scheme_byte_string_type,              /* 60 */
  scheme_unix_path_type,                /* 61 */
  scheme_windows_path_type,             /* 62 */
  scheme_symbol_type,                   /* 63 */
  scheme_keyword_type,                  /* 64 */
  scheme_null_type,                     /* 65 */
  scheme_pair_type,                     /* 66 */
  scheme_mutable_pair_type,             /* 67 */
  scheme_vector_type,                   /* 68 */
  scheme_inspector_type,                /* 69 */
  scheme_input_port_type,               /* 70 */
  scheme_output_port_type,              /* 71 */
  scheme_eof_type,                      /* 72 */
  scheme_true_type,                     /* 73 */
  scheme_false_type,                    /* 74 */
  scheme_void_type,                     /* 75 */
  scheme_primitive_syntax_type,         /* 76 */
  scheme_macro_type,                    /* 77 */
  scheme_box_type,                      /* 78 */
  scheme_thread_type,                   /* 79 */
  scheme_scope_type,                    /* 80 */
  scheme_stx_offset_type,               /* 81 */
  scheme_cont_mark_set_type,            /* 82 */
  scheme_sema_type,                     /* 83 */
  scheme_hash_table_type,               /* 84 */
  scheme_hash_tree_type,                /* 85 */
  scheme_eq_hash_tree_type,             /* 86 */
  scheme_eqv_hash_tree_type,            /* 87 */
  scheme_hash_tree_subtree_type,        /* 88 */
  scheme_hash_tree_collision_type,      /* 89 */
  scheme_hash_tree_indirection_type,    /* 90 */
  scheme_cpointer_type,                 /* 91 */
  scheme_prefix_type,                   /* 92 */
  scheme_weak_box_type,                 /* 93 */
  scheme_ephemeron_type,                /* 94 */
  scheme_struct_type_type,              /* 95 */
  scheme_module_index_type,             /* 96 */
  scheme_set_macro_type,                /* 97 */
  scheme_listener_type,                 /* 98 */
  scheme_namespace_type,                /* 99 */
  scheme_config_type,                   /* 100 */
  scheme_stx_type,                      /* 101 */
  scheme_will_executor_type,            /* 102 */
  scheme_custodian_type,                /* 103 */
  scheme_random_state_type,             /* 104 */
  scheme_regexp_type,                   /* 105 */
  scheme_bucket_type,                   /* 106 */
  scheme_bucket_table_type,             /* 107 */
  scheme_subprocess_type,               /* 108 */
  scheme_compilation_top_type,          /* 109 */
  scheme_wrap_chunk_type,               /* 110 */
  scheme_eval_waiting_type,             /* 111 */
  scheme_tail_call_waiting_type,        /* 112 */
  scheme_undefined_type,                /* 113 */
  scheme_struct_property_type,          /* 114 */
  scheme_chaperone_property_type,       /* 115 */
  scheme_multiple_values_type,          /* 116 */
  scheme_placeholder_type,              /* 117 */
  scheme_table_placeholder_type,        /* 118 */
  scheme_scope_table_type,              /* 119 */
  scheme_propagate_table_type,          /* 120 */
  scheme_svector_type,                  /* 121 */
  scheme_resolve_prefix_type,           /* 122 */
  scheme_security_guard_type,           /* 123 */
  scheme_indent_type,                   /* 124 */
  scheme_udp_type,                      /* 125 */
  scheme_udp_evt_type,                  /* 126 */
  scheme_tcp_accept_evt_type,           /* 127 */
  scheme_id_macro_type,                 /* 128 */
  scheme_evt_set_type,                  /* 129 */
  scheme_wrap_evt_type,                 /* 130 */
  scheme_handle_evt_type,               /* 131 */
  scheme_replace_evt_type,              /* 132 */
  scheme_active_replace_evt_type,       /* 133 */
  scheme_nack_guard_evt_type,           /* 134 */
  scheme_semaphore_repost_type,         /* 135 */
  scheme_channel_type,                  /* 136 */
  scheme_channel_put_type,              /* 137 */
  scheme_thread_resume_type,            /* 138 */
  scheme_thread_suspend_type,           /* 139 */
  scheme_thread_dead_type,              /* 140 */
  scheme_poll_evt_type,                 /* 141 */
  scheme_nack_evt_type,                 /* 142 */
  scheme_module_registry_type,          /* 143 */
  scheme_thread_set_type,               /* 144 */
  scheme_string_converter_type,         /* 145 */
  scheme_alarm_type,                    /* 146 */
  scheme_thread_recv_evt_type,          /* 147 */
  scheme_thread_cell_type,              /* 148 */
  scheme_channel_syncer_type,           /* 149 */
  scheme_special_comment_type,          /* 150 */
  scheme_write_evt_type,                /* 151 */
  scheme_always_evt_type,               /* 152 */
  scheme_never_evt_type,                /* 153 */
  scheme_progress_evt_type,             /* 154 */
  scheme_place_dead_type,               /* 155 */
  scheme_already_comp_type,             /* 156 */
  scheme_readtable_type,                /* 157 */
  scheme_intdef_context_type,           /* 158 */
  scheme_lexical_rib_type,              /* 159 */
  scheme_thread_cell_values_type,       /* 160 */
  scheme_global_ref_type,               /* 161 */
  scheme_cont_mark_chain_type,          /* 162 */
  scheme_raw_pair_type,                 /* 163 */
  scheme_prompt_type,                   /* 164 */
  scheme_prompt_tag_type,               /* 165 */
  scheme_continuation_mark_key_type,    /* 166 */
  scheme_expanded_syntax_type,          /* 167 */
  scheme_delay_syntax_type,             /* 168 */
  scheme_cust_box_type,                 /* 169 */
  scheme_resolved_module_path_type,     /* 170 */
  scheme_module_phase_exports_type,     /* 171 */
  scheme_logger_type,                   /* 172 */
  scheme_log_reader_type,               /* 173 */
  scheme_marshal_share_type,            /* 174 */
  scheme_rib_delimiter_type,            /* 175 */
  scheme_noninline_proc_type,           /* 176 */
  scheme_prune_context_type,            /* 177 */
  scheme_future_type,                   /* 178 */
  scheme_flvector_type,                 /* 179 */
  scheme_extflvector_type,              /* 180 */
  scheme_fxvector_type,                 /* 181 */
  scheme_place_type,                    /* 182 */
  scheme_place_object_type,             /* 183 */
  scheme_place_async_channel_type,      /* 184 */
  scheme_place_bi_channel_type,         /* 185 */
  scheme_once_used_type,                /* 186 */
  scheme_serialized_symbol_type,        /* 187 */
  scheme_serialized_keyword_type,       /* 188 */
  scheme_serialized_structure_type,     /* 189 */
  scheme_fsemaphore_type,               /* 190 */
  scheme_serialized_tcp_fd_type,        /* 191 */
  scheme_serialized_file_fd_type,       /* 192 */
  scheme_port_closed_evt_type,          /* 193 */
  scheme_proc_shape_type,               /* 194 */
  scheme_struct_proc_shape_type,        /* 195 */
  scheme_phantom_bytes_type,            /* 196 */
  scheme_environment_variables_type,    /* 197 */
  scheme_filesystem_change_evt_type,    /* 198 */
  scheme_ctype_type,                    /* 199 */
  scheme_plumber_type,                  /* 200 */
  scheme_plumber_handle_type,           /* 201 */
  scheme_deferred_expr_type,            /* 202 */
  scheme_will_be_lambda_type,           /* 203 */
  scheme_syntax_property_preserve_type, /* 204 */
                                        
#ifdef MZTAG_REQUIRED                            
  _scheme_last_normal_type_,            /* 205 */

  /* The remaining tags exist for GC tracing (in non-conservative
     mode), but they are not needed for run-time tag tests */
                                                 
  scheme_rt_weak_array,                 /* 206 */
                                        
  scheme_rt_comp_env,                   /* 207 */
  scheme_rt_constant_binding,           /* 208 */
  scheme_rt_resolve_info,               /* 209 */
  scheme_rt_unresolve_info,             /* 210 */
  scheme_rt_optimize_info,              /* 211 */
  scheme_rt_cont_mark,                  /* 212 */
  scheme_rt_saved_stack,                /* 213 */
  scheme_rt_reply_item,                 /* 214 */
  scheme_rt_ir_lambda_info,             /* 215 */
  scheme_rt_overflow,                   /* 216 */
  scheme_rt_overflow_jmp,               /* 217 */
  scheme_rt_meta_cont,                  /* 218 */
  scheme_rt_dyn_wind_cell,              /* 219 */
  scheme_rt_dyn_wind_info,              /* 220 */
  scheme_rt_dyn_wind,                   /* 221 */
  scheme_rt_dup_check,                  /* 222 */
  scheme_rt_thread_memory,              /* 223 */
  scheme_rt_input_file,                 /* 224 */
  scheme_rt_input_fd,                   /* 225 */
  scheme_rt_oskit_console_input,        /* 226 */
  scheme_rt_tested_input_file,          /* 227 */
  scheme_rt_tested_output_file,         /* 228 */
  scheme_rt_indexed_string,             /* 229 */
  scheme_rt_output_file,                /* 230 */
  scheme_rt_load_handler_data,          /* 231 */
  scheme_rt_pipe,                       /* 232 */
  scheme_rt_beos_process,               /* 233 */
  scheme_rt_system_child,               /* 234 */
  scheme_rt_tcp,                        /* 235 */
  scheme_rt_write_data,                 /* 236 */
  scheme_rt_tcp_select_info,            /* 237 */
  scheme_rt_param_data,                 /* 238 */
  scheme_rt_will,                       /* 239 */
  scheme_rt_linker_name,                /* 240 */
  scheme_rt_param_map,                  /* 241 */
  scheme_rt_finalization,               /* 242 */
  scheme_rt_finalizations,              /* 243 */
  scheme_rt_cpp_object,                 /* 244 */
  scheme_rt_cpp_array_object,           /* 245 */
  scheme_rt_stack_object,               /* 246 */
  scheme_rt_preallocated_object,        /* 247 */
  scheme_thread_hop_type,               /* 248 */
  scheme_rt_srcloc,                     /* 249 */
  scheme_rt_evt,                        /* 250 */
  scheme_rt_syncing,                    /* 251 */
  scheme_rt_comp_prefix,                /* 252 */
  scheme_rt_user_input,                 /* 253 */
  scheme_rt_user_output,                /* 254 */
  scheme_rt_compact_port,               /* 255 */
  scheme_rt_read_special_dw,            /* 256 */
  scheme_rt_regwork,                    /* 257 */
  scheme_rt_rx_lazy_string,             /* 258 */
  scheme_rt_buf_holder,                 /* 259 */
  scheme_rt_parameterization,           /* 260 */
  scheme_rt_print_params,               /* 261 */
  scheme_rt_read_params,                /* 262 */
  scheme_rt_native_code,                /* 263 */
  scheme_rt_native_code_plus_case,      /* 264 */
  scheme_rt_jitter_data,                /* 265 */
  scheme_rt_module_exports,             /* 266 */
  scheme_rt_delay_load_info,            /* 267 */
  scheme_rt_marshal_info,               /* 268 */
  scheme_rt_unmarshal_info,             /* 269 */
  scheme_rt_runstack,                   /* 270 */
  scheme_rt_sfs_info,                   /* 271 */
  scheme_rt_validate_clearing,          /* 272 */
  scheme_rt_lightweight_cont,           /* 273 */
  scheme_rt_export_info,                /* 274 */
  scheme_rt_cont_jmp,                   /* 275 */
  scheme_rt_letrec_check_frame,         /* 276 */
#endif

  _scheme_last_type_
};
