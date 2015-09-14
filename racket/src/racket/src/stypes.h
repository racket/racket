
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
  scheme_with_immed_mark_type,          /* 25 */
  scheme_case_lambda_sequence_type,     /* 26 */
  scheme_module_type,                   /* 27 */
  scheme_inline_variant_type,           /* 28 */

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,/* 30 */
  scheme_compiled_let_value_type,       /* 31 */
  scheme_compiled_let_void_type,        /* 32 */
  scheme_compiled_toplevel_type,        /* 33 */
  scheme_compiled_quote_syntax_type,    /* 34 */

  scheme_quote_compilation_type, /* used while writing, only */

  /* Registered in prefix table: */
  scheme_variable_type,                 /* 36 */
  scheme_module_variable_type, /* link replaces with scheme_variable_type */

  _scheme_compiled_values_types_,       /* 38 */

  /* procedure types */
  scheme_prim_type,                     /* 39 */
  scheme_closed_prim_type,              /* 40 */
  scheme_closure_type,                  /* 41 */
  scheme_case_closure_type,             /* 42 */
  scheme_cont_type,                     /* 43 */
  scheme_escaping_cont_type,            /* 44 */
  scheme_proc_struct_type,              /* 45 */
  scheme_native_closure_type,           /* 46 */
  scheme_proc_chaperone_type,           /* 47 */

  scheme_chaperone_type,                /* 48 */

  /* structure type (plus one above for procs) */
  scheme_structure_type,                /* 49 */

  /* number types (must be together) */
  scheme_integer_type,                  /* 50 */
  scheme_bignum_type,                   /* 51 */
  scheme_rational_type,                 /* 52 */
  scheme_float_type,                    /* 53 */
  scheme_double_type,                   /* 54 */
  scheme_complex_type,                  /* 55 */

  /* other eqv?-able values (must be with numbers) */
  scheme_char_type,                     /* 56 */

  /* other values */
  scheme_long_double_type,              /* 57 */
  scheme_char_string_type,              /* 58 */
  scheme_byte_string_type,              /* 59 */
  scheme_unix_path_type,                /* 60 */
  scheme_windows_path_type,             /* 61 */
  scheme_symbol_type,                   /* 62 */
  scheme_keyword_type,                  /* 63 */
  scheme_null_type,                     /* 64 */
  scheme_pair_type,                     /* 65 */
  scheme_mutable_pair_type,             /* 66 */
  scheme_vector_type,                   /* 67 */
  scheme_inspector_type,                /* 68 */
  scheme_input_port_type,               /* 69 */
  scheme_output_port_type,              /* 70 */
  scheme_eof_type,                      /* 71 */
  scheme_true_type,                     /* 72 */
  scheme_false_type,                    /* 73 */
  scheme_void_type,                     /* 74 */
  scheme_syntax_compiler_type,          /* 75 */
  scheme_macro_type,                    /* 76 */
  scheme_box_type,                      /* 77 */
  scheme_thread_type,                   /* 78 */
  scheme_scope_type,                    /* 79 */
  scheme_stx_offset_type,               /* 80 */
  scheme_cont_mark_set_type,            /* 81 */
  scheme_sema_type,                     /* 82 */
  scheme_hash_table_type,               /* 83 */
  scheme_hash_tree_type,                /* 84 */
  scheme_eq_hash_tree_type,             /* 85 */
  scheme_eqv_hash_tree_type,            /* 86 */
  scheme_hash_tree_subtree_type,        /* 87 */
  scheme_hash_tree_collision_type,      /* 88 */
  scheme_hash_tree_indirection_type,    /* 89 */
  scheme_cpointer_type,                 /* 90 */
  scheme_prefix_type,                   /* 91 */
  scheme_weak_box_type,                 /* 92 */
  scheme_ephemeron_type,                /* 93 */
  scheme_struct_type_type,              /* 94 */
  scheme_module_index_type,             /* 95 */
  scheme_set_macro_type,                /* 96 */
  scheme_listener_type,                 /* 97 */
  scheme_namespace_type,                /* 98 */
  scheme_config_type,                   /* 99 */
  scheme_stx_type,                      /* 100 */
  scheme_will_executor_type,            /* 101 */
  scheme_custodian_type,                /* 102 */
  scheme_random_state_type,             /* 103 */
  scheme_regexp_type,                   /* 104 */
  scheme_bucket_type,                   /* 105 */
  scheme_bucket_table_type,             /* 106 */
  scheme_subprocess_type,               /* 107 */
  scheme_compilation_top_type,          /* 108 */
  scheme_wrap_chunk_type,               /* 109 */
  scheme_eval_waiting_type,             /* 110 */
  scheme_tail_call_waiting_type,        /* 111 */
  scheme_undefined_type,                /* 112 */
  scheme_struct_property_type,          /* 113 */
  scheme_chaperone_property_type,       /* 114 */
  scheme_multiple_values_type,          /* 115 */
  scheme_placeholder_type,              /* 116 */
  scheme_table_placeholder_type,        /* 117 */
  scheme_scope_table_type,              /* 118 */
  scheme_propagate_table_type,          /* 119 */
  scheme_svector_type,                  /* 120 */
  scheme_resolve_prefix_type,           /* 121 */
  scheme_security_guard_type,           /* 122 */
  scheme_indent_type,                   /* 123 */
  scheme_udp_type,                      /* 124 */
  scheme_udp_evt_type,                  /* 125 */
  scheme_tcp_accept_evt_type,           /* 126 */
  scheme_id_macro_type,                 /* 127 */
  scheme_evt_set_type,                  /* 128 */
  scheme_wrap_evt_type,                 /* 129 */
  scheme_handle_evt_type,               /* 130 */
  scheme_replace_evt_type,              /* 131 */
  scheme_active_replace_evt_type,       /* 132 */
  scheme_nack_guard_evt_type,           /* 133 */
  scheme_semaphore_repost_type,         /* 134 */
  scheme_channel_type,                  /* 135 */
  scheme_channel_put_type,              /* 136 */
  scheme_thread_resume_type,            /* 137 */
  scheme_thread_suspend_type,           /* 138 */
  scheme_thread_dead_type,              /* 139 */
  scheme_poll_evt_type,                 /* 140 */
  scheme_nack_evt_type,                 /* 141 */
  scheme_module_registry_type,          /* 142 */
  scheme_thread_set_type,               /* 143 */
  scheme_string_converter_type,         /* 144 */
  scheme_alarm_type,                    /* 145 */
  scheme_thread_recv_evt_type,          /* 146 */
  scheme_thread_cell_type,              /* 147 */
  scheme_channel_syncer_type,           /* 148 */
  scheme_special_comment_type,          /* 149 */
  scheme_write_evt_type,                /* 150 */
  scheme_always_evt_type,               /* 151 */
  scheme_never_evt_type,                /* 152 */
  scheme_progress_evt_type,             /* 153 */
  scheme_place_dead_type,               /* 154 */
  scheme_already_comp_type,             /* 155 */
  scheme_readtable_type,                /* 156 */
  scheme_intdef_context_type,           /* 157 */
  scheme_lexical_rib_type,              /* 158 */
  scheme_thread_cell_values_type,       /* 159 */
  scheme_global_ref_type,               /* 160 */
  scheme_cont_mark_chain_type,          /* 161 */
  scheme_raw_pair_type,                 /* 162 */
  scheme_prompt_type,                   /* 163 */
  scheme_prompt_tag_type,               /* 164 */
  scheme_continuation_mark_key_type,    /* 165 */
  scheme_expanded_syntax_type,          /* 166 */
  scheme_delay_syntax_type,             /* 167 */
  scheme_cust_box_type,                 /* 168 */
  scheme_resolved_module_path_type,     /* 169 */
  scheme_module_phase_exports_type,     /* 170 */
  scheme_logger_type,                   /* 171 */
  scheme_log_reader_type,               /* 172 */
  scheme_marshal_share_type,            /* 173 */
  scheme_rib_delimiter_type,            /* 174 */
  scheme_noninline_proc_type,           /* 175 */
  scheme_prune_context_type,            /* 176 */
  scheme_future_type,                   /* 177 */
  scheme_flvector_type,                 /* 178 */
  scheme_extflvector_type,              /* 179 */
  scheme_fxvector_type,                 /* 180 */
  scheme_place_type,                    /* 181 */
  scheme_place_object_type,             /* 182 */
  scheme_place_async_channel_type,      /* 183 */
  scheme_place_bi_channel_type,         /* 184 */
  scheme_once_used_type,                /* 185 */
  scheme_serialized_symbol_type,        /* 186 */
  scheme_serialized_keyword_type,       /* 187 */
  scheme_serialized_structure_type,     /* 188 */
  scheme_fsemaphore_type,               /* 189 */
  scheme_serialized_tcp_fd_type,        /* 190 */
  scheme_serialized_file_fd_type,       /* 191 */
  scheme_port_closed_evt_type,          /* 192 */
  scheme_proc_shape_type,               /* 193 */
  scheme_struct_proc_shape_type,        /* 194 */
  scheme_phantom_bytes_type,            /* 195 */
  scheme_environment_variables_type,    /* 196 */
  scheme_filesystem_change_evt_type,    /* 197 */
  scheme_ctype_type,                    /* 198 */
  scheme_plumber_type,                  /* 199 */
  scheme_plumber_handle_type,           /* 200 */
                                        
#ifdef MZTAG_REQUIRED                            
  _scheme_last_normal_type_,            /* 201 */
                                                 
  scheme_rt_weak_array,                 /* 202 */
                                        
  scheme_rt_comp_env,                   /* 203 */
  scheme_rt_constant_binding,           /* 204 */
  scheme_rt_resolve_info,               /* 205 */
  scheme_rt_unresolve_info,             /* 206 */
  scheme_rt_optimize_info,              /* 207 */
  scheme_rt_cont_mark,                  /* 208 */
  scheme_rt_saved_stack,                /* 209 */
  scheme_rt_reply_item,                 /* 210 */
  scheme_rt_closure_info,               /* 211 */
  scheme_rt_overflow,                   /* 212 */
  scheme_rt_overflow_jmp,               /* 213 */
  scheme_rt_meta_cont,                  /* 214 */
  scheme_rt_dyn_wind_cell,              /* 215 */
  scheme_rt_dyn_wind_info,              /* 216 */
  scheme_rt_dyn_wind,                   /* 217 */
  scheme_rt_dup_check,                  /* 218 */
  scheme_rt_thread_memory,              /* 219 */
  scheme_rt_input_file,                 /* 220 */
  scheme_rt_input_fd,                   /* 221 */
  scheme_rt_oskit_console_input,        /* 222 */
  scheme_rt_tested_input_file,          /* 223 */
  scheme_rt_tested_output_file,         /* 224 */
  scheme_rt_indexed_string,             /* 225 */
  scheme_rt_output_file,                /* 226 */
  scheme_rt_load_handler_data,          /* 227 */
  scheme_rt_pipe,                       /* 228 */
  scheme_rt_beos_process,               /* 229 */
  scheme_rt_system_child,               /* 230 */
  scheme_rt_tcp,                        /* 231 */
  scheme_rt_write_data,                 /* 232 */
  scheme_rt_tcp_select_info,            /* 233 */
  scheme_rt_param_data,                 /* 234 */
  scheme_rt_will,                       /* 235 */
  scheme_rt_linker_name,                /* 236 */
  scheme_rt_param_map,                  /* 237 */
  scheme_rt_finalization,               /* 238 */
  scheme_rt_finalizations,              /* 239 */
  scheme_rt_cpp_object,                 /* 240 */
  scheme_rt_cpp_array_object,           /* 241 */
  scheme_rt_stack_object,               /* 242 */
  scheme_rt_preallocated_object,        /* 243 */
  scheme_thread_hop_type,               /* 244 */
  scheme_rt_srcloc,                     /* 245 */
  scheme_rt_evt,                        /* 246 */
  scheme_rt_syncing,                    /* 247 */
  scheme_rt_comp_prefix,                /* 248 */
  scheme_rt_user_input,                 /* 249 */
  scheme_rt_user_output,                /* 250 */
  scheme_rt_compact_port,               /* 251 */
  scheme_rt_read_special_dw,            /* 252 */
  scheme_rt_regwork,                    /* 253 */
  scheme_rt_rx_lazy_string,             /* 254 */
  scheme_rt_buf_holder,                 /* 255 */
  scheme_rt_parameterization,           /* 256 */
  scheme_rt_print_params,               /* 257 */
  scheme_rt_read_params,                /* 258 */
  scheme_rt_native_code,                /* 259 */
  scheme_rt_native_code_plus_case,      /* 260 */
  scheme_rt_jitter_data,                /* 261 */
  scheme_rt_module_exports,             /* 262 */
  scheme_rt_delay_load_info,            /* 263 */
  scheme_rt_marshal_info,               /* 264 */
  scheme_rt_unmarshal_info,             /* 265 */
  scheme_rt_runstack,                   /* 266 */
  scheme_rt_sfs_info,                   /* 267 */
  scheme_rt_validate_clearing,          /* 268 */
  scheme_rt_lightweight_cont,           /* 269 */
  scheme_rt_export_info,                /* 270 */
  scheme_rt_cont_jmp,                   /* 271 */
  scheme_rt_letrec_check_frame,         /* 272 */
#endif
  scheme_deferred_expr_type,            /* 273 */

  _scheme_last_type_
};
