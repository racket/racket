
enum {

  /* compiled object types: (internal) */
  scheme_toplevel_type,                 /* 0 */
  scheme_local_type,                    /* 1 */
  scheme_local_unbox_type,              /* 2 */
  scheme_application_type,              /* 3 */
  scheme_application2_type,             /* 4 */
  scheme_application3_type,             /* 5 */
  scheme_sequence_type,                 /* 6 */
  scheme_branch_type,                   /* 7 */
  scheme_unclosed_procedure_type,       /* 8 */
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
  scheme_case_lambda_sequence_type,     /* 25 */
  scheme_module_type,                   /* 26 */
  scheme_inline_variant_type,           /* 27 */

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,/* 29 */
  scheme_compiled_let_value_type,       /* 30 */
  scheme_compiled_let_void_type,        /* 31 */
  scheme_compiled_toplevel_type,        /* 32 */
  scheme_compiled_quote_syntax_type,    /* 33 */

  scheme_quote_compilation_type, /* used while writing, only */

  /* Registered in prefix table: */
  scheme_variable_type,                 /* 35 */
  scheme_module_variable_type, /* link replaces with scheme_variable_type */

  _scheme_compiled_values_types_,       /* 37 */

  /* procedure types */
  scheme_prim_type,                     /* 38 */
  scheme_closed_prim_type,              /* 39 */
  scheme_closure_type,                  /* 40 */
  scheme_case_closure_type,             /* 41 */
  scheme_cont_type,                     /* 42 */
  scheme_escaping_cont_type,            /* 43 */
  scheme_proc_struct_type,              /* 44 */
  scheme_native_closure_type,           /* 45 */
  scheme_proc_chaperone_type,           /* 46 */

  scheme_chaperone_type,                /* 47 */

  /* structure type (plus one above for procs) */
  scheme_structure_type,                /* 48 */

  /* number types (must be together) */
  scheme_integer_type,                  /* 49 */
  scheme_bignum_type,                   /* 50 */
  scheme_rational_type,                 /* 51 */
  scheme_float_type,                    /* 52 */
  scheme_double_type,                   /* 53 */
  scheme_complex_type,                  /* 54 */

  /* other eqv?-able values (must be with numbers) */
  scheme_char_type,                     /* 55 */

  /* other values */
  scheme_long_double_type,              /* 56 */
  scheme_char_string_type,              /* 57 */
  scheme_byte_string_type,              /* 58 */
  scheme_unix_path_type,                /* 59 */
  scheme_windows_path_type,             /* 60 */
  scheme_symbol_type,                   /* 61 */
  scheme_keyword_type,                  /* 62 */
  scheme_null_type,                     /* 63 */
  scheme_pair_type,                     /* 64 */
  scheme_mutable_pair_type,             /* 65 */
  scheme_vector_type,                   /* 66 */
  scheme_inspector_type,                /* 67 */
  scheme_input_port_type,               /* 68 */
  scheme_output_port_type,              /* 69 */
  scheme_eof_type,                      /* 70 */
  scheme_true_type,                     /* 71 */
  scheme_false_type,                    /* 72 */
  scheme_void_type,                     /* 73 */
  scheme_syntax_compiler_type,          /* 74 */
  scheme_macro_type,                    /* 75 */
  scheme_box_type,                      /* 76 */
  scheme_thread_type,                   /* 77 */
  scheme_stx_offset_type,               /* 78 */
  scheme_cont_mark_set_type,            /* 79 */
  scheme_sema_type,                     /* 80 */
  scheme_hash_table_type,               /* 81 */
  scheme_hash_tree_type,                /* 82 */
  scheme_cpointer_type,                 /* 83 */
  scheme_prefix_type,                   /* 84 */
  scheme_weak_box_type,                 /* 85 */
  scheme_ephemeron_type,                /* 86 */
  scheme_struct_type_type,              /* 87 */
  scheme_module_index_type,             /* 88 */
  scheme_set_macro_type,                /* 89 */
  scheme_listener_type,                 /* 90 */
  scheme_namespace_type,                /* 91 */
  scheme_config_type,                   /* 92 */
  scheme_stx_type,                      /* 93 */
  scheme_will_executor_type,            /* 94 */
  scheme_custodian_type,                /* 95 */
  scheme_random_state_type,             /* 96 */
  scheme_regexp_type,                   /* 97 */
  scheme_bucket_type,                   /* 98 */
  scheme_bucket_table_type,             /* 99 */
  scheme_subprocess_type,               /* 100 */
  scheme_compilation_top_type,          /* 101 */
  scheme_wrap_chunk_type,               /* 102 */
  scheme_eval_waiting_type,             /* 103 */
  scheme_tail_call_waiting_type,        /* 104 */
  scheme_undefined_type,                /* 105 */
  scheme_struct_property_type,          /* 106 */
  scheme_chaperone_property_type,       /* 107 */
  scheme_multiple_values_type,          /* 108 */
  scheme_placeholder_type,              /* 109 */
  scheme_table_placeholder_type,        /* 110 */
  scheme_rename_table_type,             /* 111 */
  scheme_rename_table_set_type,         /* 112 */
  scheme_svector_type,                  /* 113 */
  scheme_resolve_prefix_type,           /* 114 */
  scheme_security_guard_type,           /* 115 */
  scheme_indent_type,                   /* 116 */
  scheme_udp_type,                      /* 117 */
  scheme_udp_evt_type,                  /* 118 */
  scheme_tcp_accept_evt_type,           /* 119 */
  scheme_id_macro_type,                 /* 120 */
  scheme_evt_set_type,                  /* 121 */
  scheme_wrap_evt_type,                 /* 122 */
  scheme_handle_evt_type,               /* 123 */
  scheme_nack_guard_evt_type,           /* 124 */
  scheme_semaphore_repost_type,         /* 125 */
  scheme_channel_type,                  /* 126 */
  scheme_channel_put_type,              /* 127 */
  scheme_thread_resume_type,            /* 128 */
  scheme_thread_suspend_type,           /* 129 */
  scheme_thread_dead_type,              /* 130 */
  scheme_poll_evt_type,                 /* 131 */
  scheme_nack_evt_type,                 /* 132 */
  scheme_module_registry_type,          /* 133 */
  scheme_thread_set_type,               /* 134 */
  scheme_string_converter_type,         /* 135 */
  scheme_alarm_type,                    /* 136 */
  scheme_thread_recv_evt_type,          /* 137 */
  scheme_thread_cell_type,              /* 138 */
  scheme_channel_syncer_type,           /* 139 */
  scheme_special_comment_type,          /* 140 */
  scheme_write_evt_type,                /* 141 */
  scheme_always_evt_type,               /* 142 */
  scheme_never_evt_type,                /* 143 */
  scheme_progress_evt_type,             /* 144 */
  scheme_place_dead_type,               /* 145 */
  scheme_already_comp_type,             /* 146 */
  scheme_readtable_type,                /* 147 */
  scheme_intdef_context_type,           /* 148 */
  scheme_lexical_rib_type,              /* 149 */
  scheme_thread_cell_values_type,       /* 150 */
  scheme_global_ref_type,               /* 151 */
  scheme_cont_mark_chain_type,          /* 152 */
  scheme_raw_pair_type,                 /* 153 */
  scheme_prompt_type,                   /* 154 */
  scheme_prompt_tag_type,               /* 155 */
  scheme_continuation_mark_key_type,    /* 156 */
  scheme_expanded_syntax_type,          /* 157 */
  scheme_delay_syntax_type,             /* 158 */
  scheme_cust_box_type,                 /* 159 */
  scheme_resolved_module_path_type,     /* 160 */
  scheme_module_phase_exports_type,     /* 161 */
  scheme_logger_type,                   /* 162 */
  scheme_log_reader_type,               /* 163 */
  scheme_free_id_info_type,             /* 164 */
  scheme_rib_delimiter_type,            /* 165 */
  scheme_noninline_proc_type,           /* 166 */
  scheme_prune_context_type,            /* 167 */
  scheme_future_type,                   /* 168 */
  scheme_flvector_type,                 /* 169 */
  scheme_extflvector_type,              /* 170 */
  scheme_fxvector_type,                 /* 171 */
  scheme_place_type,                    /* 172 */
  scheme_place_object_type,             /* 173 */
  scheme_place_async_channel_type,      /* 174 */
  scheme_place_bi_channel_type,         /* 175 */
  scheme_once_used_type,                /* 176 */
  scheme_serialized_symbol_type,        /* 177 */
  scheme_serialized_keyword_type,       /* 178 */
  scheme_serialized_structure_type,     /* 179 */
  scheme_fsemaphore_type,               /* 180 */
  scheme_serialized_tcp_fd_type,        /* 181 */
  scheme_serialized_file_fd_type,       /* 182 */
  scheme_port_closed_evt_type,          /* 183 */
  scheme_proc_shape_type,               /* 184 */
  scheme_struct_proc_shape_type,        /* 185 */
  scheme_phantom_bytes_type,            /* 186 */
  scheme_environment_variables_type,    /* 187 */
  scheme_filesystem_change_evt_type,    /* 188 */
  scheme_ctype_type,                    /* 189 */
  scheme_plumber_type,                  /* 190 */
  scheme_plumber_handle_type,           /* 191 */
                                        
#ifdef MZTAG_REQUIRED                            
  _scheme_last_normal_type_,            /* 192 */
                                                 
  scheme_rt_weak_array,                 /* 193 */
                                        
  scheme_rt_comp_env,                   /* 194 */
  scheme_rt_constant_binding,           /* 195 */
  scheme_rt_resolve_info,               /* 196 */
  scheme_rt_unresolve_info,             /* 197 */
  scheme_rt_optimize_info,              /* 198 */
  scheme_rt_compile_info,               /* 199 */
  scheme_rt_cont_mark,                  /* 200 */
  scheme_rt_saved_stack,                /* 201 */
  scheme_rt_reply_item,                 /* 202 */
  scheme_rt_closure_info,               /* 203 */
  scheme_rt_overflow,                   /* 204 */
  scheme_rt_overflow_jmp,               /* 205 */
  scheme_rt_meta_cont,                  /* 206 */
  scheme_rt_dyn_wind_cell,              /* 207 */
  scheme_rt_dyn_wind_info,              /* 208 */
  scheme_rt_dyn_wind,                   /* 209 */
  scheme_rt_dup_check,                  /* 210 */
  scheme_rt_thread_memory,              /* 211 */
  scheme_rt_input_file,                 /* 212 */
  scheme_rt_input_fd,                   /* 213 */
  scheme_rt_oskit_console_input,        /* 214 */
  scheme_rt_tested_input_file,          /* 215 */
  scheme_rt_tested_output_file,         /* 216 */
  scheme_rt_indexed_string,             /* 217 */
  scheme_rt_output_file,                /* 218 */
  scheme_rt_load_handler_data,          /* 219 */
  scheme_rt_pipe,                       /* 220 */
  scheme_rt_beos_process,               /* 221 */
  scheme_rt_system_child,               /* 222 */
  scheme_rt_tcp,                        /* 223 */
  scheme_rt_write_data,                 /* 224 */
  scheme_rt_tcp_select_info,            /* 225 */
  scheme_rt_param_data,                 /* 226 */
  scheme_rt_will,                       /* 227 */
  scheme_rt_linker_name,                /* 228 */
  scheme_rt_param_map,                  /* 229 */
  scheme_rt_finalization,               /* 230 */
  scheme_rt_finalizations,              /* 231 */
  scheme_rt_cpp_object,                 /* 232 */
  scheme_rt_cpp_array_object,           /* 233 */
  scheme_rt_stack_object,               /* 234 */
  scheme_rt_preallocated_object,        /* 235 */
  scheme_thread_hop_type,               /* 236 */
  scheme_rt_srcloc,                     /* 237 */
  scheme_rt_evt,                        /* 238 */
  scheme_rt_syncing,                    /* 239 */
  scheme_rt_comp_prefix,                /* 240 */
  scheme_rt_user_input,                 /* 241 */
  scheme_rt_user_output,                /* 242 */
  scheme_rt_compact_port,               /* 243 */
  scheme_rt_read_special_dw,            /* 244 */
  scheme_rt_regwork,                    /* 245 */
  scheme_rt_rx_lazy_string,             /* 246 */
  scheme_rt_buf_holder,                 /* 247 */
  scheme_rt_parameterization,           /* 248 */
  scheme_rt_print_params,               /* 249 */
  scheme_rt_read_params,                /* 250 */
  scheme_rt_native_code,                /* 251 */
  scheme_rt_native_code_plus_case,      /* 252 */
  scheme_rt_jitter_data,                /* 253 */
  scheme_rt_module_exports,             /* 254 */
  scheme_rt_delay_load_info,            /* 255 */
  scheme_rt_marshal_info,               /* 256 */
  scheme_rt_unmarshal_info,             /* 257 */
  scheme_rt_runstack,                   /* 258 */
  scheme_rt_sfs_info,                   /* 259 */
  scheme_rt_validate_clearing,          /* 260 */
  scheme_rt_avl_node,                   /* 261 */
  scheme_rt_lightweight_cont,           /* 262 */
  scheme_rt_export_info,                /* 263 */
  scheme_rt_cont_jmp,                   /* 264 */
  scheme_rt_letrec_check_frame,         /* 265 */
#endif
  scheme_deferred_expr_type,            /* 266 */

  _scheme_last_type_
};
