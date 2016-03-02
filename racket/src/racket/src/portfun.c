/*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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

/* This file implements the least platform-specific aspects of Racket
   port types. */

#include "schpriv.h"
#include "schvers.h"

static Scheme_Object *input_port_p (int, Scheme_Object *[]);
static Scheme_Object *output_port_p (int, Scheme_Object *[]);
static Scheme_Object *port_closed_p (int, Scheme_Object *[]);
static Scheme_Object *current_input_port (int, Scheme_Object *[]);
static Scheme_Object *string_port_p(int, Scheme_Object *[]);
static Scheme_Object *current_output_port (int, Scheme_Object *[]);
static Scheme_Object *current_error_port (int, Scheme_Object *[]);
static Scheme_Object *make_input_port (int, Scheme_Object *[]);
static Scheme_Object *make_output_port (int, Scheme_Object *[]);
static Scheme_Object *open_input_file (int, Scheme_Object *[]);
static Scheme_Object *open_output_file (int, Scheme_Object *[]);
static Scheme_Object *open_input_output_file (int, Scheme_Object *[]);
static Scheme_Object *close_input_port (int, Scheme_Object *[]);
static Scheme_Object *close_output_port (int, Scheme_Object *[]);
static Scheme_Object *call_with_output_file (int, Scheme_Object *[]);
static Scheme_Object *call_with_input_file (int, Scheme_Object *[]);
static Scheme_Object *with_input_from_file (int, Scheme_Object *[]);
static Scheme_Object *with_output_to_file (int, Scheme_Object *[]);
static Scheme_Object *read_f (int, Scheme_Object *[]);
static Scheme_Object *read_recur_f (int, Scheme_Object *[]);
static Scheme_Object *read_syntax_f (int, Scheme_Object *[]);
static Scheme_Object *read_syntax_recur_f (int, Scheme_Object *[]);
static Scheme_Object *read_language (int, Scheme_Object *[]);
static Scheme_Object *read_char (int, Scheme_Object *[]);
static Scheme_Object *read_char_spec (int, Scheme_Object *[]);
static Scheme_Object *read_byte (int, Scheme_Object *[]);
static Scheme_Object *read_byte_spec (int, Scheme_Object *[]);
static Scheme_Object *read_line (int, Scheme_Object *[]);
static Scheme_Object *read_byte_line (int, Scheme_Object *[]);
static Scheme_Object *sch_read_string (int, Scheme_Object *[]);
static Scheme_Object *sch_read_string_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_string (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_string_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_read_bytes (int, Scheme_Object *[]);
static Scheme_Object *sch_read_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_bytes (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang_break (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang_break (int, Scheme_Object *[]);
static Scheme_Object *write_bytes(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_string(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_nonblock(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_write_atomic(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_provide_progress_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_write_special(int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_char (int, Scheme_Object *[]);
static Scheme_Object *peek_char_spec (int, Scheme_Object *[]);
static Scheme_Object *peek_byte (int, Scheme_Object *[]);
static Scheme_Object *peek_byte_spec (int, Scheme_Object *[]);
static Scheme_Object *eof_object_p (int, Scheme_Object *[]);
static Scheme_Object *char_ready_p (int, Scheme_Object *[]);
static Scheme_Object *byte_ready_p (int, Scheme_Object *[]);
static Scheme_Object *peeked_read(int argc, Scheme_Object *argv[]);
static Scheme_Object *progress_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *is_progress_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *closed_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_special_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_write (int, Scheme_Object *[]);
static Scheme_Object *display (int, Scheme_Object *[]);
static Scheme_Object *sch_print (int, Scheme_Object *[]);
static Scheme_Object *newline (int, Scheme_Object *[]);
static Scheme_Object *write_char (int, Scheme_Object *[]);
static Scheme_Object *write_byte (int, Scheme_Object *[]);
static Scheme_Object *load (int, Scheme_Object *[]);
static Scheme_Object *current_load (int, Scheme_Object *[]);
static Scheme_Object *current_load_use_compiled (int, Scheme_Object *[]);
static Scheme_Object *current_load_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_write_directory(int argc, Scheme_Object *argv[]);
#ifdef LOAD_ON_DEMAND
static Scheme_Object *load_on_demand_enabled(int argc, Scheme_Object *argv[]);
#endif
static Scheme_Object *default_load (int, Scheme_Object *[]);
static Scheme_Object *flush_output (int, Scheme_Object *[]);
static Scheme_Object *open_input_char_string (int, Scheme_Object *[]);
static Scheme_Object *open_input_byte_string (int, Scheme_Object *[]);
static Scheme_Object *open_output_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_char_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_byte_string (int, Scheme_Object *[]);
static Scheme_Object *sch_pipe(int, Scheme_Object **args);
static Scheme_Object *pipe_length(int, Scheme_Object **args);
static Scheme_Object *port_read_handler(int, Scheme_Object **args);
static Scheme_Object *port_display_handler(int, Scheme_Object **args);
static Scheme_Object *port_write_handler(int, Scheme_Object **args);
static Scheme_Object *port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_count_lines(int, Scheme_Object **args);
static Scheme_Object *port_count_lines(int, Scheme_Object **args);
static Scheme_Object *port_counts_lines_p(int, Scheme_Object **args);
static Scheme_Object *port_next_location(int, Scheme_Object **args);
static Scheme_Object *set_port_next_location(int, Scheme_Object **args);

static Scheme_Object *filesystem_change_evt(int, Scheme_Object **args);
static Scheme_Object *filesystem_change_evt_p(int, Scheme_Object **args);
static Scheme_Object *filesystem_change_evt_cancel(int, Scheme_Object **args);

static Scheme_Object *sch_default_read_handler(void *ignore, int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[]);

static int pipe_input_p(Scheme_Object *o);
static int pipe_output_p(Scheme_Object *o);
static int pipe_out_ready(Scheme_Output_Port *p);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

ROSYM static Scheme_Object *any_symbol;
ROSYM static Scheme_Object *any_one_symbol;
ROSYM static Scheme_Object *cr_symbol;
ROSYM static Scheme_Object *lf_symbol;
ROSYM static Scheme_Object *crlf_symbol;
ROSYM static Scheme_Object *module_symbol;
ROSYM static Scheme_Object *string_symbol;

READ_ONLY static Scheme_Object *default_read_handler;
READ_ONLY static Scheme_Object *default_display_handler;
READ_ONLY static Scheme_Object *default_write_handler;
READ_ONLY static Scheme_Object *default_print_handler;

READ_ONLY Scheme_Object *scheme_eof_object_p_proc;
READ_ONLY Scheme_Object *scheme_default_global_print_handler;

READ_ONLY Scheme_Object *scheme_write_proc;
READ_ONLY Scheme_Object *scheme_display_proc;
READ_ONLY Scheme_Object *scheme_print_proc;

SHARED_OK Scheme_Object *initial_compiled_file_paths;
SHARED_OK Scheme_Object *initial_compiled_file_roots;

THREAD_LOCAL_DECL(static Scheme_Object *dummy_input_port);
THREAD_LOCAL_DECL(static Scheme_Object *dummy_output_port);

#define fail_err_symbol scheme_false

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_port_fun(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(default_read_handler);
  REGISTER_SO(default_display_handler);
  REGISTER_SO(default_write_handler);
  REGISTER_SO(default_print_handler);

  REGISTER_SO(scheme_write_proc);
  REGISTER_SO(scheme_display_proc);
  REGISTER_SO(scheme_print_proc);

  REGISTER_SO(any_symbol);
  REGISTER_SO(any_one_symbol);
  REGISTER_SO(cr_symbol);
  REGISTER_SO(lf_symbol);
  REGISTER_SO(crlf_symbol);
  REGISTER_SO(module_symbol);
  REGISTER_SO(string_symbol);

  any_symbol      = scheme_intern_symbol("any");
  any_one_symbol  = scheme_intern_symbol("any-one");
  cr_symbol       = scheme_intern_symbol("return");
  lf_symbol       = scheme_intern_symbol("linefeed");
  crlf_symbol     = scheme_intern_symbol("return-linefeed");
  module_symbol   = scheme_intern_symbol("module");
  string_symbol   = scheme_intern_symbol("string");

  scheme_write_proc   = scheme_make_noncm_prim(sch_write, "write",    1, 2);
  scheme_display_proc = scheme_make_noncm_prim(display,   "display",  1, 2);
  scheme_print_proc   = scheme_make_noncm_prim(sch_print, "print",    1, 3);

  /* Made as a closed prim so we can get the arity right: */
  default_read_handler = scheme_make_closed_prim_w_arity(sch_default_read_handler, NULL, "default-port-read-handler", 1, 2);

  default_display_handler = scheme_make_prim_w_arity(sch_default_display_handler, "default-port-display-handler", 2, 2);
  default_write_handler   = scheme_make_prim_w_arity(sch_default_write_handler,   "default-port-write-handler",   2, 2);
  default_print_handler   = scheme_make_prim_w_arity(sch_default_print_handler,   "default-port-print-handler",   2, 3);

  scheme_add_global_constant("eof", scheme_eof, env);
  
  GLOBAL_PARAMETER("current-input-port",                current_input_port,         MZCONFIG_INPUT_PORT,  env);
  GLOBAL_PARAMETER("current-output-port",               current_output_port,        MZCONFIG_OUTPUT_PORT, env);
  GLOBAL_PARAMETER("current-error-port",                current_error_port,         MZCONFIG_ERROR_PORT,  env); 
  GLOBAL_PARAMETER("current-load",                      current_load,               MZCONFIG_LOAD_HANDLER,          env);
  GLOBAL_PARAMETER("current-load/use-compiled",         current_load_use_compiled,  MZCONFIG_LOAD_COMPILED_HANDLER, env);
  GLOBAL_PARAMETER("current-load-relative-directory",   current_load_directory,     MZCONFIG_LOAD_DIRECTORY,        env);
  GLOBAL_PARAMETER("current-write-relative-directory",  current_write_directory,    MZCONFIG_WRITE_DIRECTORY,       env);
  GLOBAL_PARAMETER("global-port-print-handler",         global_port_print_handler,  MZCONFIG_PORT_PRINT_HANDLER,    env);
#ifdef LOAD_ON_DEMAND
  GLOBAL_PARAMETER("load-on-demand-enabled",            load_on_demand_enabled,     MZCONFIG_LOAD_DELAY_ENABLED,    env);
#endif
  GLOBAL_PARAMETER("port-count-lines-enabled",          global_port_count_lines,    MZCONFIG_PORT_COUNT_LINES,      env);

  GLOBAL_FOLDING_PRIM("input-port?",            input_port_p,               1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("output-port?",           output_port_p,              1, 1, 1, env); 
  GLOBAL_FOLDING_PRIM("file-stream-port?",      scheme_file_stream_port_p,  1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("string-port?",           string_port_p,              1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("terminal-port?",         scheme_terminal_port_p,     1, 1, 1, env);

  GLOBAL_NONCM_PRIM("port-closed?",             port_closed_p,          1, 1, env); 
  GLOBAL_NONCM_PRIM("open-input-file",          open_input_file,        1, 3, env);
  GLOBAL_NONCM_PRIM("open-input-bytes",         open_input_byte_string, 1, 2, env);
  GLOBAL_NONCM_PRIM("open-input-string",        open_input_char_string, 1, 2, env);
  GLOBAL_NONCM_PRIM("open-output-file",         open_output_file,       1, 3, env);
  GLOBAL_NONCM_PRIM("open-output-bytes",        open_output_string,     0, 1, env);
  GLOBAL_NONCM_PRIM("open-output-string",       open_output_string,     0, 1, env);
  GLOBAL_NONCM_PRIM("get-output-bytes",         get_output_byte_string, 1, 4, env);
  GLOBAL_NONCM_PRIM("get-output-string",        get_output_char_string, 1, 1, env);
  GLOBAL_NONCM_PRIM("open-input-output-file",   open_input_output_file, 1, 3, env);
  GLOBAL_NONCM_PRIM("close-input-port",         close_input_port,       1, 1, env);
  GLOBAL_NONCM_PRIM("close-output-port",        close_output_port,      1, 1, env);
  GLOBAL_NONCM_PRIM("make-input-port",          make_input_port,        4, 10, env);
  GLOBAL_NONCM_PRIM("make-output-port",         make_output_port,       4, 11, env);
  
  GLOBAL_PRIM_W_ARITY2("call-with-output-file", call_with_output_file,  2, 4, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("call-with-input-file",  call_with_input_file,   2, 3, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("with-output-to-file",   with_output_to_file,    2, 4, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("with-input-from-file",  with_input_from_file,   2, 3, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("load",                  load,                   1, 1, 0, -1, env);
  GLOBAL_PRIM_W_ARITY2("make-pipe",             sch_pipe,               0, 3, 2,  2, env);
  GLOBAL_PRIM_W_ARITY2("port-next-location",    port_next_location,     1, 1, 3,  3, env);
  GLOBAL_NONCM_PRIM("set-port-next-location!",  set_port_next_location, 4, 4, env);

  GLOBAL_PRIM_W_ARITY("filesystem-change-evt",  filesystem_change_evt,   1, 2, env);
  GLOBAL_NONCM_PRIM("filesystem-change-evt?",   filesystem_change_evt_p, 1, 1, env);
  GLOBAL_NONCM_PRIM("filesystem-change-evt-cancel",  filesystem_change_evt_cancel, 1, 1, env);

  GLOBAL_NONCM_PRIM("read",                           read_f,                         0, 1, env);
  GLOBAL_NONCM_PRIM("read/recursive",                 read_recur_f,                   0, 4, env);
  GLOBAL_NONCM_PRIM("read-syntax",                    read_syntax_f,                  0, 2, env);
  GLOBAL_NONCM_PRIM("read-syntax/recursive",          read_syntax_recur_f,            0, 5, env);
  GLOBAL_PRIM_W_ARITY2("read-language",               read_language,                  0, 2, 0, -1, env);
  GLOBAL_NONCM_PRIM("read-char",                      read_char,                      0, 1, env);
  GLOBAL_NONCM_PRIM("read-char-or-special",           read_char_spec,                 0, 1, env);
  GLOBAL_NONCM_PRIM("read-byte",                      read_byte,                      0, 1, env);
  GLOBAL_NONCM_PRIM("read-byte-or-special",           read_byte_spec,                 0, 1, env);
  GLOBAL_NONCM_PRIM("read-bytes-line",                read_byte_line,                 0, 2, env);
  GLOBAL_NONCM_PRIM("read-line",                      read_line,                      0, 2, env);
  GLOBAL_NONCM_PRIM("read-string",                    sch_read_string,                1, 2, env);
  GLOBAL_NONCM_PRIM("read-string!",                   sch_read_string_bang,           1, 4, env);
  GLOBAL_NONCM_PRIM("peek-string",                    sch_peek_string,                2, 3, env);
  GLOBAL_NONCM_PRIM("peek-string!",                   sch_peek_string_bang,           2, 5, env);
  GLOBAL_NONCM_PRIM("read-bytes",                     sch_read_bytes,                 1, 2, env);
  GLOBAL_NONCM_PRIM("read-bytes!",                    sch_read_bytes_bang,            1, 4, env);
  GLOBAL_NONCM_PRIM("peek-bytes",                     sch_peek_bytes,                 2, 3, env);
  GLOBAL_NONCM_PRIM("peek-bytes!",                    sch_peek_bytes_bang,            2, 5, env);
  GLOBAL_NONCM_PRIM("read-bytes-avail!",              read_bytes_bang,                1, 4, env);
  GLOBAL_NONCM_PRIM("read-bytes-avail!*",             read_bytes_bang_nonblock,       1, 4, env);
  GLOBAL_NONCM_PRIM("read-bytes-avail!/enable-break", read_bytes_bang_break,          1, 4, env);
  GLOBAL_NONCM_PRIM("peek-bytes-avail!",              peek_bytes_bang,                2, 6, env);
  GLOBAL_NONCM_PRIM("peek-bytes-avail!*",             peek_bytes_bang_nonblock,       2, 6, env);
  GLOBAL_NONCM_PRIM("peek-bytes-avail!/enable-break", peek_bytes_bang_break,          2, 6, env);
  GLOBAL_NONCM_PRIM("port-provides-progress-evts?",   can_provide_progress_evt,       1, 1, env);
  GLOBAL_NONCM_PRIM("write-bytes",                    write_bytes,                    1, 4, env);
  GLOBAL_NONCM_PRIM("write-string",                   write_string,                   1, 4, env);
  GLOBAL_NONCM_PRIM("write-bytes-avail",              write_bytes_avail,              1, 4, env);
  GLOBAL_NONCM_PRIM("write-bytes-avail*",             write_bytes_avail_nonblock,     1, 4, env);
  GLOBAL_NONCM_PRIM("write-bytes-avail/enable-break", write_bytes_avail_break,        1, 4, env);
  GLOBAL_NONCM_PRIM("port-writes-atomic?",            can_write_atomic,               1, 1, env);
  GLOBAL_NONCM_PRIM("port-writes-special?",           can_write_special,              1, 1, env);
  GLOBAL_NONCM_PRIM("write-special",                  scheme_write_special,           1, 2, env);
  GLOBAL_NONCM_PRIM("write-special-avail*",           scheme_write_special_nonblock,  1, 2, env);
  GLOBAL_NONCM_PRIM("peek-char",                      peek_char,                      0, 2, env);
  GLOBAL_NONCM_PRIM("peek-char-or-special",           peek_char_spec,                 0, 2, env);
  GLOBAL_NONCM_PRIM("peek-byte",                      peek_byte,                      0, 2, env);
  GLOBAL_NONCM_PRIM("peek-byte-or-special",           peek_byte_spec,                 0, 3, env);
  GLOBAL_NONCM_PRIM("byte-ready?",                    byte_ready_p,                   0, 1, env);
  GLOBAL_NONCM_PRIM("char-ready?",                    char_ready_p,                   0, 1, env);
  GLOBAL_NONCM_PRIM("newline",                        newline,                        0, 1, env);
  GLOBAL_NONCM_PRIM("write-char",                     write_char,                     1, 2, env);
  GLOBAL_NONCM_PRIM("write-byte",                     write_byte,                     1, 2, env);
  GLOBAL_NONCM_PRIM("port-commit-peeked",             peeked_read,                    3, 4, env);
  GLOBAL_NONCM_PRIM("port-progress-evt",              progress_evt,                   0, 1, env);
  GLOBAL_NONCM_PRIM("progress-evt?",                  is_progress_evt,                1, 2, env);
  GLOBAL_NONCM_PRIM("port-closed-evt",                closed_evt,                     0, 1, env);
  GLOBAL_NONCM_PRIM("write-bytes-avail-evt",          write_bytes_avail_evt,          1, 4, env);
  GLOBAL_NONCM_PRIM("write-special-evt",              write_special_evt,              2, 2, env);
  GLOBAL_NONCM_PRIM("port-read-handler",              port_read_handler,              1, 2, env);
  GLOBAL_NONCM_PRIM("port-display-handler",           port_display_handler,           1, 2, env);
  GLOBAL_NONCM_PRIM("port-write-handler",             port_write_handler,             1, 2, env);
  GLOBAL_NONCM_PRIM("port-print-handler",             port_print_handler,             1, 2, env);
  GLOBAL_NONCM_PRIM("flush-output",                   flush_output,                   0, 1, env);
  GLOBAL_NONCM_PRIM("file-position",                  scheme_file_position,           1, 2, env);
  GLOBAL_NONCM_PRIM("file-position*",                 scheme_file_position_star,      1, 1, env);
  GLOBAL_NONCM_PRIM("file-truncate",                  scheme_file_truncate,           2, 2, env);
  GLOBAL_NONCM_PRIM("file-stream-buffer-mode",        scheme_file_buffer,             1, 2, env);
  GLOBAL_NONCM_PRIM("port-try-file-lock?",            scheme_file_try_lock,           2, 2, env);
  GLOBAL_NONCM_PRIM("port-file-unlock",               scheme_file_unlock,             1, 1, env);
  GLOBAL_NONCM_PRIM("port-file-identity",             scheme_file_identity,           1, 1, env);
  GLOBAL_NONCM_PRIM("port-count-lines!",              port_count_lines,               1, 1, env);
  GLOBAL_NONCM_PRIM("port-counts-lines?",             port_counts_lines_p,            1, 1, env);
          
  REGISTER_SO(scheme_eof_object_p_proc);
  scheme_eof_object_p_proc = scheme_make_folding_prim(eof_object_p, "eof-object?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(scheme_eof_object_p_proc) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                                                   | SCHEME_PRIM_IS_OMITABLE);
  scheme_add_global_constant("eof-object?", scheme_eof_object_p_proc, env);

  scheme_add_global_constant("write",   scheme_write_proc,    env);
  scheme_add_global_constant("display", scheme_display_proc,  env);
  scheme_add_global_constant("print",   scheme_print_proc,    env);

  GLOBAL_IMMED_PRIM("pipe-content-length",              pipe_length,                1, 1, env);

  REGISTER_SO(scheme_default_global_print_handler);
  scheme_default_global_print_handler
    = scheme_make_prim_w_arity(sch_default_global_port_print_handler, "default-global-port-print-handler", 2, 3);
}


void scheme_init_port_fun_config(void)
{
  scheme_set_root_param(MZCONFIG_LOAD_DIRECTORY, scheme_false);
  scheme_set_root_param(MZCONFIG_WRITE_DIRECTORY, scheme_false);
  if (initial_compiled_file_paths)
    scheme_set_root_param(MZCONFIG_USE_COMPILED_KIND, initial_compiled_file_paths);
  else
    scheme_set_root_param(MZCONFIG_USE_COMPILED_KIND, scheme_make_pair(scheme_make_path("compiled"), scheme_null));
  if (initial_compiled_file_roots)
    scheme_set_root_param(MZCONFIG_USE_COMPILED_ROOTS, initial_compiled_file_roots);
  else
    scheme_set_root_param(MZCONFIG_USE_COMPILED_ROOTS, scheme_make_pair(scheme_intern_symbol("same"), scheme_null));
  scheme_set_root_param(MZCONFIG_USE_USER_PATHS, (scheme_ignore_user_paths ? scheme_false : scheme_true));
  scheme_set_root_param(MZCONFIG_USE_LINK_PATHS, (scheme_ignore_link_paths ? scheme_false : scheme_true));

  {
    Scheme_Object *dlh;
    dlh = scheme_make_prim_w_arity2(default_load, "default-load-handler", 2, 2, 0, -1);
    scheme_set_root_param(MZCONFIG_LOAD_HANDLER, dlh);
  }

  scheme_set_root_param(MZCONFIG_PORT_PRINT_HANDLER, scheme_default_global_print_handler);

  /* Use dummy port: */
  REGISTER_SO(dummy_input_port);
  REGISTER_SO(dummy_output_port);
  dummy_input_port = scheme_make_byte_string_input_port("");
  dummy_output_port = scheme_make_null_output_port(1);
}

void scheme_set_compiled_file_paths(Scheme_Object *list)
{
  if (!initial_compiled_file_paths)
    REGISTER_SO(initial_compiled_file_paths);
  initial_compiled_file_paths = list;
}

void scheme_set_compiled_file_roots(Scheme_Object *list)
{
  if (!initial_compiled_file_roots)
    REGISTER_SO(initial_compiled_file_roots);
  initial_compiled_file_roots = list;
}

/*========================================================================*/
/*                              port records                              */
/*========================================================================*/

Scheme_Port *scheme_port_record(Scheme_Object *port)
{
  if (scheme_is_input_port(port))
    return (Scheme_Port *)scheme_input_port_record(port);
  else
    return (Scheme_Port *)scheme_output_port_record(port);
}

static MZ_INLINE Scheme_Input_Port *input_port_record_slow(Scheme_Object *port)
{
  Scheme_Object *v;

  while (1) {
    if (SCHEME_INPORTP(port))
      return (Scheme_Input_Port *)port;

    if (!SCHEME_CHAPERONE_STRUCTP(port)) {
      return (Scheme_Input_Port *)dummy_input_port;
    }
    
    v = scheme_struct_type_property_ref(scheme_input_port_property, port);
    if (!v)
      v = scheme_false;
    else if (SCHEME_INTP(v))
      v = scheme_struct_ref(port, SCHEME_INT_VAL(v));
    port = v;

    SCHEME_USE_FUEL(1);
  }
}

Scheme_Input_Port *scheme_input_port_record(Scheme_Object *port)
  XFORM_ASSERT_NO_CONVERSION
{
  /* Avoid MZ_PRECISE_GC instrumentation in the common case: */
  if (SCHEME_INPORTP(port))
    return (Scheme_Input_Port *)port;
  else
    return input_port_record_slow(port);
}

static MZ_INLINE Scheme_Output_Port *output_port_record_slow(Scheme_Object *port)
{
  Scheme_Object *v;

  while (1) {
    if (SCHEME_OUTPORTP(port))
      return (Scheme_Output_Port *)port;

    if (!SCHEME_CHAPERONE_STRUCTP(port)) {
      return (Scheme_Output_Port *)dummy_output_port;
    }
    
    v = scheme_struct_type_property_ref(scheme_output_port_property, port);
    if (!v)
      v = scheme_false;
    else if (SCHEME_INTP(v))
      v = scheme_struct_ref(port, SCHEME_INT_VAL(v));
    port = v;

    SCHEME_USE_FUEL(1);
  }
}

Scheme_Output_Port *scheme_output_port_record(Scheme_Object *port)
  XFORM_ASSERT_NO_CONVERSION
{
  /* Avoid MZ_PRECISE_GC instrumentation in the common case: */
  if (SCHEME_OUTPORTP(port))
    return (Scheme_Output_Port *)port;
  else
    return output_port_record_slow(port);
}

int scheme_is_input_port(Scheme_Object *port)
{
  if (SCHEME_INPORTP(port))
    return 1;

  if (SCHEME_CHAPERONE_STRUCTP(port))
    if (scheme_struct_type_property_ref(scheme_input_port_property, port))
      return 1;

  return 0;
}

int scheme_is_output_port(Scheme_Object *port)
{
  if (SCHEME_OUTPORTP(port))
    return 1;
  
  if (SCHEME_CHAPERONE_STRUCTP(port))
    if (scheme_struct_type_property_ref(scheme_output_port_property, port))
      return 1;

  return 0;
}

/*========================================================================*/
/*                          string input ports                            */
/*========================================================================*/

static intptr_t
string_get_or_peek_bytes(Scheme_Input_Port *port,
			 char *buffer, intptr_t offset, intptr_t size,
			 int peek, intptr_t skip,
			 Scheme_Object *unless)
{
  Scheme_Indexed_String *is;

  if (unless && scheme_unless_ready(unless))
    return SCHEME_UNLESS_READY;

  is = (Scheme_Indexed_String *) port->port_data;
  if (is->index + skip >= is->size)
    return EOF;
  else if (size == 1) {
    int pos = is->index;
    if (buffer)
      buffer[offset] = is->string[pos + skip];
    if (!peek)
      is->index = pos + 1;
    return 1;
  } else {
    intptr_t l, delta;

    delta = is->index + skip;

    if (delta + size <= is->size)
      l = size;
    else
      l = (is->size - delta);

    if (buffer)
      memcpy(buffer + offset, is->string + delta, l);
    if (!peek)
      is->index += l;

    return l;
  }
}

static intptr_t
string_get_bytes(Scheme_Input_Port *port,
		  char *buffer, intptr_t offset, intptr_t size,
		  int nonblock, Scheme_Object *unless)
{
  return string_get_or_peek_bytes(port, buffer, offset, size, 0, 0, unless);
}

static intptr_t
string_peek_bytes(Scheme_Input_Port *port,
		   char *buffer, intptr_t offset, intptr_t size,
		   Scheme_Object *sskip,
		   int nonblock, Scheme_Object *unless)
{
  intptr_t skip;

  if (SCHEME_INTP(sskip))
    skip = SCHEME_INT_VAL(sskip);
  else
    skip = ((Scheme_Indexed_String *)port->port_data)->size;

  return string_get_or_peek_bytes(port, buffer, offset, size, 1, skip, unless);
}

static int
string_byte_ready (Scheme_Input_Port *port)
{
  return 1;
}

static void
string_close_in (Scheme_Input_Port *port)
{
}

static Scheme_Indexed_String *
make_indexed_string (const char *str, intptr_t len)
{
  Scheme_Indexed_String *is;

  is = MALLOC_ONE_RT(Scheme_Indexed_String);
#ifdef MZTAG_REQUIRED
  is->type = scheme_rt_indexed_string;
#endif

  if (str) {
    if (len < 0) {
      is->string = (char *)str;
      is->size = -len;
    } else {
      char *ca;
      ca = (char *)scheme_malloc_atomic(len);
      is->string = ca;
      memcpy(is->string, str, len);
      is->size = len;
    }
  } else {
    char *ca;
    is->size = 100;
    ca = (char *)scheme_malloc_atomic(is->size + 1);
    is->string = ca;
  }
  is->index = 0;
  return (is);
}

Scheme_Object *
scheme_make_sized_byte_string_input_port(const char *str, intptr_t len)
{
  Scheme_Input_Port *ip;

  ip = scheme_make_input_port(scheme_string_input_port_type,
			      make_indexed_string(str, len),
			      string_symbol,
			      string_get_bytes,
			      string_peek_bytes,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      string_byte_ready,
			      string_close_in,
			      NULL,
			      0);

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_byte_string_input_port(const char *str)
{
  return scheme_make_sized_byte_string_input_port(str, strlen(str));
}

/*========================================================================*/
/*                          string output ports                           */
/*========================================================================*/

static intptr_t
string_write_bytes(Scheme_Output_Port *port,
		    const char *str, intptr_t d, intptr_t len,
		    int rarely_block, int enable_break)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;

  if (is->index + len >= is->size) {
    char *old;

    old = is->string;

    if (len > is->size)
      is->size += 2 * len;
    else
      is->size *= 2;

    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(is->size + 1);
      is->string = ca;
    }
    memcpy(is->string, old, is->index);
  }

  memcpy(is->string + is->index, str + d, len);
  is->index += len;

  return len;
}

static void
string_close_out (Scheme_Output_Port *port)
{
  return;
}

Scheme_Object *
scheme_make_byte_string_output_port (void)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port (scheme_string_output_port_type,
				make_indexed_string(NULL, 0),
				scheme_intern_symbol("string"),
				scheme_write_evt_via_write,
				string_write_bytes,
				NULL,
				string_close_out,
				NULL,
				NULL,
				NULL,
				0);

  return (Scheme_Object *)op;
}

char *
scheme_get_reset_sized_byte_string_output(Scheme_Object *port, intptr_t *size, int reset, intptr_t startpos, intptr_t endpos)
{
  Scheme_Output_Port *op;
  Scheme_Indexed_String *is;
  char *v;
  intptr_t len;

  if (!SCHEME_OUTPUT_PORTP(port))
    return NULL;

  op = scheme_output_port_record(port);
  if (op->sub_type != scheme_string_output_port_type)
    return NULL;

  is = (Scheme_Indexed_String *)op->port_data;

  len = is->index;
  if (is->u.hot > len)
    len = is->u.hot;

  if (endpos < 0)
    endpos = len;

  if (reset) {
    char *ca;
    v = is->string;
    is->size = 31;
    ca = (char *)scheme_malloc_atomic((is->size) + 1);
    is->string = ca;
    is->index = 0;
    is->u.hot = 0;
    if ((startpos > 0) || (endpos < len)) {
      len = endpos - startpos;
      ca = (char *)scheme_malloc_atomic(len + 1);
      memcpy(ca, v XFORM_OK_PLUS startpos, len);
      v = ca;
    }
  } else {
    len = endpos - startpos;
    v = (char *)scheme_malloc_atomic(len + 1);
    memcpy(v, is->string XFORM_OK_PLUS startpos, len);
  }
  v[len] = 0;

  if (size)
    *size = len;

  return v;
}

char *
scheme_get_sized_byte_string_output(Scheme_Object *port, intptr_t *size)
{
  return scheme_get_reset_sized_byte_string_output(port, size, 0, 0, -1);
}

char *
scheme_get_string_output(Scheme_Object *port)
{
  return scheme_get_sized_byte_string_output(port, NULL);
}

/*========================================================================*/
/*                 "user" input ports (created from Racket)               */
/*========================================================================*/

typedef struct User_Input_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *evt;
  Scheme_Object *read_proc;
  Scheme_Object *peek_proc;          /* NULL => implement via read_proc */
  Scheme_Object *close_proc;
  Scheme_Object *progress_evt_proc;  /* NULL => no support for progress events */
  Scheme_Object *peeked_read_proc;   /* NULL => progress_evt_proc is NULL */
  Scheme_Object *location_proc;
  Scheme_Object *count_lines_proc;
  Scheme_Object *buffer_mode_proc;
  Scheme_Object *reuse_str;
  Scheme_Object *peeked;
  Scheme_Object *prefix_pipe;
} User_Input_Port;

#define MAX_USER_INPUT_REUSE_SIZE 1024

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Result checking                                             */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This function is mainly responsible for checking the result of a
   read-proc or peek-proc. */

static intptr_t user_read_result(const char *who, Scheme_Input_Port *port,
			     Scheme_Object *val, Scheme_Object *bstr,
			     int peek, int nonblock, int evt_ok,
			     int special_ok, int false_ok,
			     Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *a[2];

  val_again:

  if (SCHEME_EOFP(val))
    return EOF;
  else {
    int n;

    if (!SCHEME_INTP(val) || (SCHEME_INT_VAL(val) < 0)) {
      a[0] = val;
      if (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val)) {
	n = -1;
      } else if (peek && SCHEME_FALSEP(val)) {
	if (false_ok)
	  return SCHEME_UNLESS_READY;
	scheme_contract_error(who,
                              "returned #f when no progress evt was supplied",
                              NULL);
	return 0;
      } else if (SCHEME_PROCP(val)
                 && scheme_check_proc_arity(NULL, 4, 0, 1, a)) {
	if (!special_ok) {
          scheme_contract_error(who,
                                "the port has no specific peek procedure, so"
                                " a special read result is not allowed",
                                "special result", 1, val,
                                NULL);
          return 0;
        }
        port->special = val;
        return SCHEME_SPECIAL;
      } else if (evt_ok && pipe_input_p(val)) {
        ((User_Input_Port *)port->port_data)->prefix_pipe = val;
        return 0;
      } else if (evt_ok && scheme_is_evt(val)) {
	/* A peek/read failed, and we were given a evt that unblocks
	   when the read/peek (at some offset) succeeds. */
	if (nonblock > 0) {
	  if (sinfo) {
	    scheme_set_sync_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1, NULL);
	    return 0;
	  } else {
	    /* Poll: */
	    a[0] = scheme_make_integer(0);
	    a[1] = val;
	    val = scheme_sync_timeout(2, a);
	    if (!val)
	      return 0;
	    else if (scheme_is_evt(val))
	      return 0;
	    goto val_again;
	  }
	} else {
	  /* Sync on the given evt. */
	  a[0] = val;
	  if (nonblock < 0)
	    val = scheme_sync_enable_break(1, a);
	  else
	    val = scheme_sync(1, a);

	  /* Port may have been closed while we were syncing: */
	  if (port->closed) {
	    /* Another thread closed the input port while we were syncing. */
	    /* Call scheme_getc to signal the error */
	    if (peek)
	      scheme_peek_byte((Scheme_Object *)port);
	    else
	      scheme_get_byte((Scheme_Object *)port);
	    return 0; /* doesn't get here */
	  }
	  goto val_again;
	}
      } else {
	val = NULL;
	n = 0;
      }

      if (!val) {
	scheme_wrong_contract(who,
                              (peek
                               ? (evt_ok
                                  ? (special_ok
                                     ? "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f procedure?)"
                                     : "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? #f)")
                                  : "(or/c exact-nonnegative-integer? eof-object? procedure? #f)")
                               : (evt_ok
                                  ? (special_ok
                                     ? "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port? procedure?)"
                                     : "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port?)")
                                  : "(or/c exact-nonnegative-integer? eof-object? procedure?)")),
                              -1, -1, a);
	return 0;
      }
    } else
      n = SCHEME_INT_VAL(val);

    if ((n < 0) || (n > SCHEME_BYTE_STRLEN_VAL(bstr))) {
      scheme_contract_error(who,
                            "result integer is larger than the supplied byte string",
                            "result", 1, val,
                            "byte-string length", 1, scheme_make_integer(SCHEME_BYTE_STRLEN_VAL(bstr)),
                            NULL);
    }

    return n;
  }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Main reader                                                 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Call read-proc or peek-proc. */

static intptr_t
user_get_or_peek_bytes(Scheme_Input_Port *port,
		       char *buffer, intptr_t offset, intptr_t size,
		       int nonblock,
		       int peek, Scheme_Object *peek_skip,
		       Scheme_Object *unless,
		       Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *fun, *val, *a[3], *bstr;
  User_Input_Port *uip = (User_Input_Port *)port->port_data;
  intptr_t r;
  Scheme_Cont_Frame_Data cframe;

  if (peek)
    fun = uip->peek_proc;
  else
    fun = uip->read_proc;

  if (SCHEME_INPUT_PORTP(fun)) {
    return scheme_redirect_get_or_peek_bytes(port,
                                             scheme_input_port_record(fun),
                                             buffer, offset, size,
                                             nonblock,
                                             peek, peek_skip,
                                             unless, sinfo);
  }

  val = uip->peeked;
  if (val) {
    /* Leftover from a read-based peek used to implement `char-ready?'
       This can't happen is peek is 1, because in that case we have a
       peek_proc, so there's no need for read-based peeks. Also,
       SCHEME_CDR(unless) must be NULL. */
    uip->peeked = NULL;
    if (SCHEME_INTP(val)) {
      buffer[offset] = (char)SCHEME_INT_VAL(val);
      return 1;
    } else if (SCHEME_VOIDP(val)) {
      return SCHEME_SPECIAL;
    } else
      return EOF;
  }

  if (unless && SCHEME_PAIRP(unless))
    unless = SCHEME_CDR(unless);

  while (1) {
    int nb;

    if (uip->prefix_pipe) {
      /* Guarantee: if we call into a client, then we're not using the
         pipe anywhere. */
      r = scheme_pipe_char_count(uip->prefix_pipe);
      if (r && (!peek || (SCHEME_INTP(peek_skip) && (SCHEME_INT_VAL(peek_skip) < r)))) {
        /* Need atomic to ensure the guarantee: this thread shouldn't get 
           swapped out while it's using the pipe, because another thread might
           somehow arrive at the port's procedures. (Pipe reading is probably atomic, 
           anyway, due to the way that pipes are implemented.) */
        scheme_start_atomic();
        r = scheme_get_byte_string_unless("custom-port-pipe-read", uip->prefix_pipe,
                                          buffer, offset, size,
                                          2, peek, peek_skip,
                                          unless);
        scheme_end_atomic_no_swap();
        return r;
      } else {
        /* Setting the pipe to NULL ensures that we don't start using it while
           we're in the call that we just started. If another thread returns
           a pipe before the user's code returns, though, all bets are off. */
        uip->prefix_pipe = NULL;
      }
    }

    if (uip->reuse_str && (size == SCHEME_BYTE_STRLEN_VAL(uip->reuse_str))) {
      bstr = uip->reuse_str;
      uip->reuse_str = NULL;
    } else {
      char *vb;
      vb = scheme_malloc_atomic(size + 1);
      memset(vb, 0, size + 1); /* must initialize for security */
      bstr = scheme_make_sized_byte_string(vb, size, 0);
    }
    a[0] = bstr;
    a[1] = peek_skip;
    a[2] = unless ? unless : scheme_false;

    nb = nonblock;
    if (!nb) {
      if (scheme_can_break(scheme_current_thread)) {
	nb = -1;
      }
    }

    /* Disable breaks while calling the port's function: */
    scheme_push_break_enable(&cframe, 0, 0);

    /* Call the read/peek function: */
    val = scheme_apply(fun, peek ? 3 : 1, a);
    
    if ((size <= MAX_USER_INPUT_REUSE_SIZE)
	&& (SCHEME_INTP(val) || SCHEME_EOFP(val) || SCHEME_PROCP(val))) {
      uip->reuse_str = bstr;
    }

    r = user_read_result(peek ? "user port peek" : "user port read",
			 port, val, bstr, peek, nb,
			 1, !!uip->peek_proc, !!unless, sinfo);

    scheme_pop_break_enable(&cframe, 1);

    if (r > 0) {
      memcpy(buffer + offset, SCHEME_BYTE_STR_VAL(bstr), r);
      return r;
    } else if (r) {
      return r;
    }

    scheme_thread_block_enable_break(0.0, nonblock < 0); /* penalty for inaccuracy? */
    scheme_current_thread->ran_some = 1;
    /* but don't loop forever due to inaccurracy */
    if (nonblock > 0) {
      if (sinfo)
	sinfo->spin = 1;
      return 0;
    }
  }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Main entry points                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static intptr_t
user_get_bytes(Scheme_Input_Port *port,
	       char *buffer, intptr_t offset, intptr_t size,
	       int nonblock, Scheme_Object *unless)
{
  return user_get_or_peek_bytes(port, buffer, offset, size,
				nonblock, 0, NULL,
				unless,
				NULL);
}

static intptr_t
user_peek_bytes(Scheme_Input_Port *port,
		char *buffer, intptr_t offset, intptr_t size,
		Scheme_Object *skip,
		int nonblock, Scheme_Object *unless)
{
  return user_get_or_peek_bytes(port, buffer, offset, size,
				nonblock, 1, skip,
				unless,
				NULL);
}

static int
user_peeked_read(Scheme_Input_Port *port,
		 intptr_t size,
		 Scheme_Object *unless_evt,
		 Scheme_Object *target_evt)
{
  User_Input_Port *uip = (User_Input_Port *)port->port_data;
  Scheme_Object *val, *a[3];
  Scheme_Cont_Frame_Data cframe;

  /* FIXME, if possible: the peeked-read procedure should not
     synchronize target_evt more than once. There doesn't seem to
     be a way to enforce this constraint, however, without extra
     machinery in Racket's synchronization. */

  a[0] = scheme_make_integer(size);
  a[1] = unless_evt;
  a[2] = target_evt;

  /* Disable breaks while calling the port's function: */
  scheme_push_break_enable(&cframe, 0, 0);

  /* Call the read/peek function: */
  val = scheme_apply(uip->peeked_read_proc, 3, a);

  scheme_pop_break_enable(&cframe, 1);

  if (SCHEME_TRUEP(val)) {
    char *buf;

    if (SCHEME_BYTE_STRINGP(val)) {
      size = SCHEME_BYTE_STRLEN_VAL(val);
      buf = SCHEME_BYTE_STR_VAL(val);
    } else
      buf = NULL;
  
    if (port->p.count_lines) {
      if (!buf) {
        buf = scheme_malloc_atomic(size);
        memset(buf, 'x', size);
      }
    }

    scheme_port_count_lines((Scheme_Port *)port, buf, 0, size);
  }

  return SCHEME_TRUEP(val);
}

static Scheme_Object *
user_progress_evt(Scheme_Input_Port *port)
{
  User_Input_Port *uip = (User_Input_Port *)port->port_data;
  Scheme_Object *evt, *a[1];

  evt = _scheme_apply(uip->progress_evt_proc, 0, NULL);

  if (!scheme_is_evt(evt)) {
    a[0] = evt;
    scheme_wrong_contract("user port progress-evt", "evt?", -1, -1, a);
    return 0;
  }

  return evt;
}

static int
user_byte_ready_sinfo(Scheme_Input_Port *port, Scheme_Schedule_Info *sinfo)
{
  int c, can_peek;
  char s[1];
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  /* We implement char-ready? by a non-blocking peek for a single
     character. If the port provides a precise waitable, it
     effectively determines the result, because the peek function
     checks the waitable. */

  can_peek = (uip->peek_proc ? 1 : 0);

  c = user_get_or_peek_bytes(port, s, 0, 1,
			     1, can_peek, scheme_make_integer(0),
			     NULL,
			     sinfo);

  if (c == EOF) {
    if (!can_peek)
      uip->peeked = scheme_true;
    return 1;
  } else if (c) {
    if (!can_peek) {
      if (c == SCHEME_SPECIAL)
	uip->peeked = scheme_void;
      else
	uip->peeked = scheme_make_integer(s[0]);
    }
    return 1;
  } else
    return 0;
}

static int
user_byte_ready(Scheme_Input_Port *port)
{
  return user_byte_ready_sinfo(port, NULL);
}

int scheme_user_port_byte_probably_ready(Scheme_Input_Port *ip, Scheme_Schedule_Info *sinfo)
{
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  if (uip->peeked)
    return 1;

   if (sinfo->false_positive_ok) {
    /* Causes the thread to swap in: */
    sinfo->potentially_false_positive = 1;
    return 1;
  } else {
    return user_byte_ready_sinfo(ip, sinfo);
  }
}

static void
user_needs_wakeup_input (Scheme_Input_Port *port, void *fds)
{
  /* Nothing... */
}

static void
user_close_input(Scheme_Input_Port *port)
{
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  scheme_apply_multi(uip->close_proc, 0, NULL);
}

static Scheme_Object *
user_input_location(Scheme_Port *p)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  return scheme_apply_multi(uip->location_proc, 0, NULL);
}

static void
user_input_count_lines(Scheme_Port *p)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  scheme_apply_multi(uip->count_lines_proc, 0, NULL);
}


static int
user_buffer_mode(Scheme_Object *buffer_mode_proc, int mode, int line_ok)
{
  Scheme_Object *v, *a[1];

  if (mode < 0) {
    v = scheme_apply(buffer_mode_proc, 0, NULL);
    if (SCHEME_TRUEP(v)) {
      if (SAME_OBJ(v, scheme_block_symbol))
	mode = 0;
      else if (line_ok && SAME_OBJ(v, scheme_line_symbol))
	mode = 1;
      else if (SAME_OBJ(v, scheme_none_symbol))
	mode = 2;
      else {
	a[0] = v;
	scheme_wrong_contract("user port buffer-mode", 
                              line_ok ? "(or/c 'block 'line 'none #f)" : "(or/c 'block 'none #f)",
                              -1, -1, a);
	return 0;
      }
    }
  } else {
    switch (mode) {
    case 0:
      a[0] = scheme_block_symbol;
      break;
    case 1:
      a[0] = scheme_line_symbol;
      break;
    case 2:
      a[0] = scheme_none_symbol;
      break;
    }
    scheme_apply_multi(buffer_mode_proc, 1, a);
  }

  return mode;
}

static int
user_input_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  return user_buffer_mode(uip->buffer_mode_proc, mode, 0);
}

/*========================================================================*/
/*                 "user" output ports (created from Racket)              */
/*========================================================================*/

typedef struct User_Output_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *evt;
  Scheme_Object *write_evt_proc;
  Scheme_Object *write_proc;
  Scheme_Object *flush_proc;
  Scheme_Object *close_proc;
  Scheme_Object *write_special_evt_proc;
  Scheme_Object *write_special_proc;
  Scheme_Object *location_proc;
  Scheme_Object *count_lines_proc;
  Scheme_Object *buffer_mode_proc;
  Scheme_Object *buffer_pipe;
} User_Output_Port;

int scheme_user_port_write_probably_ready(Scheme_Output_Port *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  if (port->closed)
    return 1;

  val = uop->evt;

  scheme_set_sync_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1, NULL);
  return 0;

}

static int
user_write_ready(Scheme_Output_Port *port)
{
  /* This function should never be called. If we are ready-checking as
     a evt, then scheme_user_port_write_probably_ready is called,
     instead. */
  return 1;
}

static intptr_t
user_write_result(const char *who, Scheme_Output_Port *port, int evt_ok,
		  Scheme_Object *val, int rarely_block, int enable_break, intptr_t len)
{
  Scheme_Object *p[2];

  while (1) {
    if (SCHEME_FALSEP(val)) {
      if (!rarely_block)
	return 0; /* #f result is allowed, though not preferred */
      else if (rarely_block == 2)
	return -1;
      else if (!evt_ok)
	scheme_contract_error(who,
                              "bad result for write event",
                              "result", 1, val,
                              NULL);
      else 
	return 0;
    } else if (SCHEME_INTP(val)
	       && (SCHEME_INT_VAL(val) >= 0)
	       && (SCHEME_INT_VAL(val) <= len)) {
      int n;

      n = SCHEME_INT_VAL(val);

      if (!n && len) {
	scheme_contract_error(who,
                              (evt_ok
                               ? "bad result for non-flush write"
                               : "bad result for non-flush write event"),
                              "result", 1, val,
                              NULL);
      }

      if (!len && !rarely_block)
	return 1; /* turn 0 into 1 to indicate a successful blocking flush */
      else
	return n;
    } else if (evt_ok && pipe_output_p(val)) {
      if (rarely_block || !len) {
        scheme_contract_error(who,
                              (rarely_block
                               ? "bad result for a non-blocking write"
                               : "bad result for a flushing write"),
                              "result", 1, val,
                              NULL);
      }

      ((User_Output_Port *)port->port_data)->buffer_pipe = val;
      
      return 0;
    } else if (evt_ok && scheme_is_evt(val)) {
      /* A write failed, and we were given a evt that unblocks when
	 the write succeeds. */
      if (rarely_block == 2) {
	return 0;
      } else {
	/* Sync on the given evt. */
	p[0] = val;
	if (enable_break)
	  val = scheme_sync_enable_break(1, p);
	else
	  val = scheme_sync(1, p);

	/* Port may have been closed while we were syncing: */
	if (port->closed)
	  return 0;
      }
    } else {
      if ((SCHEME_INTP(val) && (SCHEME_INT_VAL(val) > 0))
	  || (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val))) {
	scheme_contract_error(who,
                              "result integer is larger than the supplied byte string",
                              "result", 1, val,
                              "byte string length", 1, scheme_make_integer(len),
                              NULL);
      } else {
	p[0] = val;
	scheme_wrong_contract(who,
                              "(or/c exact-nonnegative-integer? #f evt?)",
                              -1, -1, p);
      }
      return 0;
    }
  }
}

static intptr_t
user_write_bytes(Scheme_Output_Port *port, const char *str, intptr_t offset, intptr_t len,
		  int rarely_block, int enable_break)
{
  /* As always, rarely_block => flush, !len => flush,
     rarely_block == 1 => len > 0 */
  Scheme_Object *p[5], *to_write, *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;
  int n, re_enable_break;
  Scheme_Cont_Frame_Data cframe;

  if (SCHEME_OUTPUT_PORTP(uop->write_proc)) {
    return scheme_redirect_write_bytes(scheme_output_port_record(uop->write_proc),
                                       str, offset, len, 
                                       rarely_block, enable_break);
  }

  if (rarely_block)
    re_enable_break = 0;
  else if (enable_break)
    re_enable_break = 1;
  else
    re_enable_break = scheme_can_break(scheme_current_thread);

  to_write = scheme_make_sized_offset_byte_string((char *)str, offset, len, 1);
  p[0] = to_write;
  SCHEME_SET_IMMUTABLE(p[0]);
  p[1] = scheme_make_integer(0);
  p[2] = scheme_make_integer(len);
  p[3] = (rarely_block ? scheme_true : scheme_false);
  p[4] = (re_enable_break ? scheme_true : scheme_false);

  while (1) {

    if (uop->buffer_pipe) {
      if (!rarely_block && len) {
        if (pipe_out_ready((Scheme_Output_Port *)uop->buffer_pipe)) {
          /* Need atomic for same reason as using prefix_pipe for input. */
          scheme_start_atomic();
          n = scheme_put_byte_string("user output pipe buffer", uop->buffer_pipe,
                                     str, offset, len,
                                     1);
          scheme_end_atomic_no_swap();
          return n;
        }
      }
      uop->buffer_pipe = NULL;
    }

    /* Disable breaks while calling the port's function: */
    scheme_push_break_enable(&cframe, 0, 0);

    val = scheme_apply(uop->write_proc, 5, p);

    scheme_pop_break_enable(&cframe, 1); /* might break */

    n = user_write_result("user port write", port,
			  1, val, rarely_block, enable_break, len);

    if (!n && !rarely_block) {
      /* Try blocking write/flush again */
    } else {
      if (n || (rarely_block != 1)) {
	if (!rarely_block && !len)
	  return 0; /* n == 1 for success, but caller wants 0 */
	return n;
      }
      /* else rarely_block == 1, and we haven't written anything,
         or rarely_block == 0 and we haven't written anything but we
         received a pipe. */
    }

    scheme_thread_block(0.0);
    scheme_current_thread->ran_some = 1;
  }
}

static Scheme_Object *user_write_evt_wrapper(void *d, int argc, struct Scheme_Object *argv[])
{
  Scheme_Object *val, *port;
  intptr_t r, len;

  port = (Scheme_Object *)((void **)d)[0];
  val = (Scheme_Object *)((void **)d)[1];
  len = SCHEME_INT_VAL(val);
  val = argv[0];

  r = user_write_result("user port write-evt", (Scheme_Output_Port *)port,
			0, val, 1, 0, len);

  if (!r && len) {
    /* Port must have been closed */
    scheme_contract_error("user port write-evt",
                          "port is closed",
                          "port", 1, port,
                          NULL);
  }

  return scheme_make_integer(r);
}

static Scheme_Object *
user_write_bytes_evt(Scheme_Output_Port *port,
		     const char *buffer, intptr_t offset, intptr_t size)
{
  Scheme_Object *to_write, *wrapper;
  Scheme_Object *a[3], *val;
  void **args;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  to_write = scheme_make_sized_offset_byte_string((char *)buffer, offset, size, 1);
  SCHEME_SET_IMMUTABLE(to_write);
  a[0] = to_write;
  a[1] = scheme_make_integer(0);
  a[2] = scheme_make_integer(size);
  val = scheme_apply(uop->write_evt_proc, 3, a);

  if (!scheme_is_evt(val)) {
    a[0] = val;
    scheme_wrong_contract("user port write-evt", "evt?", -1, -1, a);
    return NULL;
  }

  /* Wrap the evt for result checking: */
  args = MALLOC_N(void*, 2);
  args[0] = port;
  args[1] = scheme_make_integer(size);
  wrapper = scheme_make_closed_prim(user_write_evt_wrapper, args);

  a[0] = val;
  a[1] = wrapper;
  return scheme_wrap_evt(2, a);
}

static void
user_needs_wakeup_output (Scheme_Output_Port *port, void *fds)
{
  /* Nothing needed. */
}

static void
user_close_output (Scheme_Output_Port *port)
{
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  scheme_apply_multi(uop->close_proc, 0, NULL);
}

static int
user_write_special (Scheme_Output_Port *port, Scheme_Object *v, int nonblock)
{
  Scheme_Object *a[3];
  User_Output_Port *uop = (User_Output_Port *)port->port_data;
  int re_enable_break;
  Scheme_Cont_Frame_Data cframe;

  if (SCHEME_OUTPUT_PORTP(uop->write_special_proc)) {
    return scheme_redirect_write_special(scheme_output_port_record(uop->write_special_proc),
                                         v,
                                         nonblock);
  }

  if (nonblock)
    re_enable_break = 0;
  else
    re_enable_break = scheme_can_break(scheme_current_thread);

  a[0] = v;
  a[1] = (nonblock ? scheme_true : scheme_false);
  a[2] = (re_enable_break ? scheme_true : scheme_false);

  scheme_push_break_enable(&cframe, 0, 0);

  v = scheme_apply(uop->write_special_proc, 3, a);

  while (1) {
    if (uop->buffer_pipe)
      uop->buffer_pipe = NULL;

    if (scheme_is_evt(v)) {
      if (!nonblock) {
	a[0] = v;
	if (re_enable_break)
	  v = scheme_sync_enable_break(1, a);
	else
	  v = scheme_sync(1, a);
      } else
	return 0;
    } else
      break;
  }

  scheme_pop_break_enable(&cframe, 1);

  return SCHEME_TRUEP(v);
}

static Scheme_Object*
user_write_special_evt (Scheme_Output_Port *port, Scheme_Object *v)
{
  Scheme_Object *a[1];
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  a[0] = v;
  v = scheme_apply(uop->write_special_evt_proc, 1, a);

  if (!scheme_is_evt(v)) {
    a[0] = v;
    scheme_wrong_contract("user port write-special-evt", "evt?", -1, -1, a);
  }

  return v;
}

static Scheme_Object *
user_output_location(Scheme_Port *p)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;
  User_Output_Port *uop = (User_Output_Port *)op->port_data;

  return scheme_apply_multi(uop->location_proc, 0, NULL);
}

static void
user_output_count_lines(Scheme_Port *p)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;
  User_Output_Port *uop = (User_Output_Port *)op->port_data;

  scheme_apply_multi(uop->count_lines_proc, 0, NULL);
}

static int
user_output_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;
  User_Output_Port *uop = (User_Output_Port *)op->port_data;

  return user_buffer_mode(uop->buffer_mode_proc, mode, 1);
}

int scheme_is_user_port(Scheme_Object *port)
{
  if (SCHEME_INPUT_PORTP(port)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(port);
    return SAME_OBJ(scheme_user_input_port_type, ip->sub_type);
  } else {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(port);
    return SAME_OBJ(scheme_user_output_port_type, op->sub_type);
  }
}

/*========================================================================*/
/*                               pipe ports                               */
/*========================================================================*/

static void pipe_did_read(Scheme_Input_Port *port, Scheme_Pipe *pipe, int peek)
{
  if (port && port->progress_evt && !peek) {
    scheme_post_sema_all(port->progress_evt);
    port->progress_evt = NULL;
  }

  while (SCHEME_PAIRP(pipe->wakeup_on_read)) {
    Scheme_Object *sema;
    sema = SCHEME_CAR(pipe->wakeup_on_read);
    pipe->wakeup_on_read = SCHEME_CDR(pipe->wakeup_on_read);
    scheme_post_sema(sema);
  }
}

static void pipe_did_write(Scheme_Pipe *pipe)
{
  while (SCHEME_PAIRP(pipe->wakeup_on_write)) {
    Scheme_Object *sema;
    sema = SCHEME_CAR(pipe->wakeup_on_write);
    pipe->wakeup_on_write = SCHEME_CDR(pipe->wakeup_on_write);
    scheme_post_sema(sema);
  }
}

static intptr_t pipe_get_or_peek_bytes(Scheme_Input_Port *p,
				   char *buffer, intptr_t offset, intptr_t size,
				   int nonblock,
				   int peek, intptr_t peek_skip,
				   Scheme_Object *unless)
{
  Scheme_Pipe *pipe;
  intptr_t c, skipped = 0;

  pipe = (Scheme_Pipe *)(p->port_data);

  while ((pipe->bufstart == pipe->bufend) && !pipe->eof) {
    if (nonblock > 0)
      return 0;

    scheme_block_until_unless((Scheme_Ready_Fun)scheme_byte_ready_or_user_port_ready,
			      NULL, (Scheme_Object *)p,
			      0.0, unless,
			      nonblock);

    scheme_wait_input_allowed(p, nonblock);

    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;
  }

  if (p->closed) {
    /* Another thread closed the input port while we were syncing. */
    /* Call scheme_getc to signal the error */
    scheme_getc((Scheme_Object *)p);
    return 0; /* doesn't get here */
  }

  if (pipe->bufstart == pipe->bufend)
    c = EOF;
  else {
    intptr_t bs = pipe->bufstart;
    c = 0;
    if (bs > pipe->bufend) {
      int n;

      /* Determine how much to copy: */
      n = pipe->buflen - bs;
      if (n < peek_skip) {
	peek_skip -= n;
	bs += n;
	skipped += n;
	n = 0;
      } else {
	bs += peek_skip;
	n -= peek_skip;
	skipped += peek_skip;
	peek_skip = 0;
      }
      if (n > size)
	n = size;

      /* Copy it */
      if (buffer)
	memcpy(buffer + offset, pipe->buf + bs, n);

      /* Fix up indices */
      bs += n;
      if (bs == pipe->buflen)
	bs = 0;
      if (!peek)
	pipe->bufstart = bs;
      size -= n;
      c += n;
    }
    if (bs < pipe->bufend) {
      int n;

      /* Determine how much to copy: */
      n = pipe->bufend - bs;
      if (n < peek_skip) {
	peek_skip -= n;
	bs += n;
	skipped += n;
	n = 0;
      } else {
	bs += peek_skip;
	n -= peek_skip;
	skipped += peek_skip;
	peek_skip = 0;
      }
      if (n > size)
	n = size;

      /* Copy it */
      if (buffer)
	memcpy(buffer + offset + c, pipe->buf + bs, n);

      /* Fix up indices */
      bs += n;
      if (!peek)
	pipe->bufstart = bs;
      size -= n;
      c += n;
    }
  }

  if (!peek && (c > 0)) {
    if (pipe->bufmaxextra) {
      if (pipe->bufmaxextra > c)
	pipe->bufmaxextra -= c;
      else
	pipe->bufmaxextra = 0;
    }
    pipe_did_read(p, pipe, 0);
  } else {
    if (!c) {
      if (size && pipe->eof)
	return EOF;
      if (!nonblock) {
	/* must have skipped too far;
	   need to sleep until chars are ready */
	Scheme_Object *my_sema, *wp;
	my_sema = scheme_make_sema(0);
	wp = scheme_make_pair(my_sema, pipe->wakeup_on_write);
	pipe->wakeup_on_write = wp;
	scheme_wait_sema(my_sema, (nonblock < 0) ? -1 : 0);
      }
    } else if (c > 0) {
      if (pipe->bufmax) {
	if (pipe->bufmaxextra < c + skipped) {
	  pipe->bufmaxextra = c + skipped;
	}
      }
      pipe_did_read(p, pipe, 1);
    }
  }

  return c;
}

static intptr_t pipe_get_bytes(Scheme_Input_Port *p,
			   char *buffer, intptr_t offset, intptr_t size,
			   int nonblock,
			   Scheme_Object *unless)
{
  return pipe_get_or_peek_bytes(p, buffer, offset, size, nonblock, 0, 0, unless);
}

static intptr_t pipe_peek_bytes(Scheme_Input_Port *p,
			    char *buffer, intptr_t offset, intptr_t size,
			    Scheme_Object *skip,
			    int nonblock,
			    Scheme_Object *unless)
{
  intptr_t peek_skip;

  if (SCHEME_INTP(skip))
    peek_skip = SCHEME_INT_VAL(skip);
  else {
#ifdef SIXTY_FOUR_BIT_INTEGERS
    peek_skip = 0x7FFFFFFFFFFFFFFF;
#else
    peek_skip = 0x7FFFFFFF;
#endif
  }

  return pipe_get_or_peek_bytes(p, buffer, offset, size, nonblock, 1, peek_skip, unless);
}

static intptr_t pipe_write_bytes(Scheme_Output_Port *p,
			      const char *str, intptr_t d, intptr_t len,
			      int rarely_block, int enable_break)
{
  Scheme_Pipe *pipe;
  intptr_t avail, firstpos, firstn, secondn, endpos;
  intptr_t wrote = 0;

  pipe = (Scheme_Pipe *)(p->port_data);

 try_again:

  if (pipe->eof || !len)
    return len + wrote;

  if (pipe->bufstart <= pipe->bufend) {
    firstn = pipe->buflen - pipe->bufend;
    avail = firstn + pipe->bufstart - 1;
    if (!pipe->bufstart)
      --firstn;
  } else {
    firstn = avail = pipe->bufstart - pipe->bufend - 1;
  }
  firstpos = pipe->bufend;

  if (pipe->bufmax) {
    /* If we've peek in the past, then buflen might have grown larger
       than bufmax. But for consistency, use that extra space only for
       peeks. */
    intptr_t extra;
    extra = pipe->buflen - (pipe->bufmax + pipe->bufmaxextra);
    if (extra > 0)
      avail -= extra;
  }

  if (pipe->bufmax && (avail < len)) {
    /* Must we block to write it all? */
    intptr_t xavail = avail;
    intptr_t can_extra;

    can_extra = ((pipe->bufmax + pipe->bufmaxextra) - pipe->buflen);
    if (can_extra > 0)
      xavail += can_extra;

    if (xavail < len) {
      /* We must block to write it all. */
      Scheme_Object *my_sema;

      /* First, write as much as seems immediately possible. */
      xavail = pipe_write_bytes(p, str, d, xavail, rarely_block, enable_break);
      wrote += xavail;
      d += xavail;
      len -= xavail;
      pipe_did_write(pipe);

      /* For non-blocking mode, that might be good enough.
	 rarely_block == 2 means that even nothing is good enough. */
      if ((rarely_block && wrote) || (rarely_block == 2))
	return wrote;

      /* Now, wait until we can write more, then start over. */
      if (pipe->bufstart <= pipe->bufend) {
        avail = (pipe->buflen - pipe->bufend) + pipe->bufstart - 1;
      } else {
        avail = pipe->bufstart - pipe->bufend - 1;
      }
      if (pipe->bufmax) {
        /* Again, it's possible that the port grew to accommodate
           past peeks... */
        intptr_t extra;
        extra = pipe->buflen - (pipe->bufmax + pipe->bufmaxextra);
        if (extra > 0)
          avail -= extra;
      }
      
      if (avail || pipe->eof || p->closed)
        goto try_again;
      
      my_sema = scheme_make_sema(0);
      {
        Scheme_Object *wp;
        wp = scheme_make_pair(my_sema, pipe->wakeup_on_read);
        pipe->wakeup_on_read = wp;
      }
      
      scheme_wait_sema(my_sema, enable_break ? -1 : 0);
      
      goto try_again;
    }
  }

  if (avail < len) {
    unsigned char *old;
    int newlen;

    old = pipe->buf;
    newlen = 2 * (pipe->buflen + len);
    if (pipe->bufmax && (newlen > (pipe->bufmax + pipe->bufmaxextra)))
      newlen = pipe->bufmax + pipe->bufmaxextra;

    {
      unsigned char *uca;
      uca = (unsigned char *)scheme_malloc_atomic(newlen);
      pipe->buf = uca;
    }

    if (pipe->bufstart <= pipe->bufend) {
      memcpy(pipe->buf, old + pipe->bufstart, pipe->bufend - pipe->bufstart);
      pipe->bufend -= pipe->bufstart;
      pipe->bufstart = 0;
    } else {
      int slen;
      slen = pipe->buflen - pipe->bufstart;
      memcpy(pipe->buf, old + pipe->bufstart, slen);
      memcpy(pipe->buf + slen, old, pipe->bufend);
      pipe->bufstart = 0;
      pipe->bufend += slen;
    }

    pipe->buflen = newlen;

    firstpos = pipe->bufend;
    firstn = len;
    endpos = firstpos + firstn;

    secondn = 0;
  } else {
    if (firstn >= len) {
      firstn = len;
      endpos = (firstpos + len) % pipe->buflen;
      secondn = 0;
    } else {
      secondn = len - firstn;
      endpos = secondn;
    }
  }

  if (firstn)
    memcpy(pipe->buf + firstpos, str + d, firstn);
  if (secondn)
    memcpy(pipe->buf, str + d + firstn, secondn);

  pipe->bufend = endpos;

  pipe_did_write(pipe);
  
  return len + wrote;
}

static int pipe_byte_ready(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;
  int v;

  pipe = (Scheme_Pipe *)(p->port_data);

  v = (pipe->bufstart != pipe->bufend || pipe->eof);

  return v;
}

static void pipe_in_close(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);

  pipe->eof = 1;

  /* to wake up any other threads blocked on pipe I/O: */
  pipe_did_read(p, pipe, 0);
  pipe_did_write(pipe);
}

static void pipe_out_close(Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);

  pipe->eof = 1;

  /* to wake up any other threads blocked on pipe I/O: */
  pipe_did_read(NULL, pipe, 0);
  pipe_did_write(pipe);
}

static int pipe_out_ready(Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;
  intptr_t avail;

  pipe = (Scheme_Pipe *)(p->port_data);

  if (pipe->eof || !pipe->bufmax)
    return 1;

  if (pipe->bufend >= pipe->bufstart) {
    avail = pipe->bufend - pipe->bufstart;
  } else {
    avail = pipe->bufend + (pipe->buflen - pipe->bufstart);
  }

  avail = pipe->bufmax + pipe->bufmaxextra - 1 - avail;

  return avail > 0;
}

void scheme_pipe_with_limit(Scheme_Object **read, Scheme_Object **write, int queuelimit)
{
  Scheme_Pipe *pipe;
  Scheme_Input_Port *readp;
  Scheme_Output_Port *writep;
  Scheme_Object *name;

  if (queuelimit) queuelimit++; /* need separator in circular buffer */

  pipe = MALLOC_ONE_RT(Scheme_Pipe);
#ifdef MZTAG_REQUIRED
  pipe->type = scheme_rt_pipe;
#endif
  pipe->buflen = ((queuelimit && (queuelimit < 100)) ? queuelimit : 100);
  {
    unsigned char *uca;
    uca = (unsigned char *)scheme_malloc_atomic(pipe->buflen);
    pipe->buf = uca;
  }
  pipe->bufstart = pipe->bufend = 0;
  pipe->eof = 0;
  pipe->bufmax = queuelimit;
  pipe->wakeup_on_read = scheme_null;
  pipe->wakeup_on_write = scheme_null;

  name = scheme_intern_symbol("pipe");

  readp = scheme_make_input_port(scheme_pipe_read_port_type,
				 (void *)pipe,
				 name,
				 pipe_get_bytes,
				 pipe_peek_bytes,
				 scheme_progress_evt_via_get,
				 scheme_peeked_read_via_get,
				 pipe_byte_ready,
				 pipe_in_close,
				 NULL,
				 0);

  writep = scheme_make_output_port(scheme_pipe_write_port_type,
				   (void *)pipe,
				   name,
				   scheme_write_evt_via_write,
				   pipe_write_bytes,
				   pipe_out_ready,
				   pipe_out_close,
				   NULL,
				   NULL,
				   NULL,
				   0);

  *read = (Scheme_Object *)readp;
  *write = (Scheme_Object *)writep;
}

void scheme_pipe(Scheme_Object **read, Scheme_Object **write)
{
  scheme_pipe_with_limit(read, write, 0);
}

static Scheme_Object *sch_pipe(int argc, Scheme_Object **args)
{
  Scheme_Object *v[2];
  int bufmax;

  if (argc == 1) {
    Scheme_Object *o = args[0];
    if (SCHEME_FALSEP(o)) {
      bufmax = 0;
    } else if ((SCHEME_INTP(o) || SCHEME_BIGNUMP(o))
               && scheme_is_positive(o)) {
      if (SCHEME_INTP(o))
	bufmax = SCHEME_INT_VAL(o);
      else
	bufmax = 0;
    } else {
      scheme_wrong_contract("make-pipe", "(or/c exact-positive-integer? #f)", 0, argc, args);
      return NULL;
    }
  } else
    bufmax = 0;

  scheme_pipe_with_limit(&v[0], &v[1], bufmax);

  if (argc > 1)
    ((Scheme_Input_Port *)(v[0]))->name = args[1];
  if (argc > 2)
    ((Scheme_Output_Port *)(v[1]))->name = args[2];

  return scheme_values(2, v);
}

static Scheme_Object *pipe_length(int argc, Scheme_Object **argv)
{
  Scheme_Object *o;
  Scheme_Pipe *pipe = NULL;
  int avail;

  o = argv[0];
  if (SCHEME_OUTPUT_PORTP(o)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(o);
    if (op->sub_type == scheme_pipe_write_port_type) {
      pipe = (Scheme_Pipe *)op->port_data;
    }
  } else if (SCHEME_INPUT_PORTP(o)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(o);
    if (ip->sub_type == scheme_pipe_read_port_type) {
      pipe = (Scheme_Pipe *)ip->port_data;
    }
  }

  if (!pipe) {
    scheme_wrong_contract("pipe-content-length",
                          "(or/c pipe-input-port? pipe-output-port?)",
		      0, argc, argv);
    return NULL;
  }
    
  if (pipe->bufend >= pipe->bufstart) {
    avail = pipe->bufend - pipe->bufstart;
  } else {
    avail = pipe->bufend + (pipe->buflen - pipe->bufstart);
  }

  return scheme_make_integer(avail);
}

static int pipe_input_p(Scheme_Object *o)
{
  /* Need an immediate pipe: */
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_input_port_type)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(o);
    if (ip->sub_type == scheme_pipe_read_port_type) {
      return 1;
    }
  }

  return 0;
}

static int pipe_output_p(Scheme_Object *o)
{
  /* Need an immediate pipe: */
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_output_port_type)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(o);
    if (op->sub_type == scheme_pipe_write_port_type) {
      return 1;
    }
  }

  return 0;
}

/*========================================================================*/
/*                    Racket functions and helpers                        */
/*========================================================================*/

static Scheme_Object *
input_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_INPUT_PORTP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
output_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_OUTPUT_PORTP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *port_closed_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_INPUT_PORTP(v)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(v);
    return ip->closed ? scheme_true : scheme_false;
  } else if (SCHEME_OUTPUT_PORTP(v)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(v);
    return op->closed ? scheme_true : scheme_false;
  } else {
    scheme_wrong_contract("port-closed?", "port?", 0, argc, argv);
    return NULL;
  }
}

intptr_t scheme_port_closed_p (Scheme_Object *port) {
  return (port_closed_p(1, &port) == scheme_false) ? 0 : 1;
}

static Scheme_Object *current_input_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-input-port", scheme_make_integer(MZCONFIG_INPUT_PORT),
                              argc, argv,
                              -1, input_port_p, "input-port?", 0);
}

static Scheme_Object *current_output_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-output-port", scheme_make_integer(MZCONFIG_OUTPUT_PORT),
                              argc, argv,
                              -1, output_port_p, "output-port?", 0);
}

static Scheme_Object *current_error_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-error-port", scheme_make_integer(MZCONFIG_ERROR_PORT),
                              argc, argv,
                              -1, output_port_p, "output-port?", 0);
}

static Scheme_Object *
make_input_port(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;
  User_Input_Port *uip;
  Scheme_Object *name;
  int read_port, peek_port;

  read_port = SCHEME_INPUT_PORTP(argv[1]);
  if (!read_port
      && !scheme_check_proc_arity(NULL, 1, 1, argc, argv)) { /* read */
    scheme_wrong_contract("make-input-port", "(or/c (procedure-arity-includes/c 1) input-port?)", 1, argc, argv);
  }
  peek_port = SCHEME_INPUT_PORTP(argv[2]);
  if (!peek_port
      && !scheme_check_proc_arity2(NULL, 3, 2, argc, argv, 1)) { /* peek */
    scheme_wrong_contract("make-input-port", "(or/c (procedure-arity-includes/c 3) input-port?)", 2, argc, argv);
  }
  scheme_check_proc_arity("make-input-port", 0, 3, argc, argv); /* close */
  if (argc > 4)
    scheme_check_proc_arity2("make-input-port", 0, 4, argc, argv, 1); /* progress-evt */
  if (argc > 5)
    scheme_check_proc_arity2("make-input-port", 3, 5, argc, argv, 1); /* peeked-read */
  if (argc > 6)
    scheme_check_proc_arity2("make-input-port", 0, 6, argc, argv, 1); /* location */
  if (argc > 7)
    scheme_check_proc_arity("make-input-port", 0, 7, argc, argv); /* count-lines! */
  if (argc > 8) { /* position */
    if (!((SCHEME_INTP(argv[8]) && SCHEME_INT_VAL(argv[8]) > 0)
	  || (SCHEME_BIGNUMP(argv[8]) && SCHEME_BIGPOS(argv[8]))
          || SCHEME_FALSEP(argv[8])
          || scheme_check_proc_arity(NULL, 0, 8, argc, argv)
          || SCHEME_INPUT_PORTP(argv[8])
          || SCHEME_OUTPUT_PORTP(argv[8])))
      scheme_wrong_contract("make-input-port", "(or/c exact-positive-integer? port? #f (-> (or/c exact-positive-integer? #f)))", 8, argc, argv);
  }
  if (argc > 9) { /* buffer-mode */
    if (SCHEME_TRUEP(argv[9])
	&& !scheme_check_proc_arity(NULL, 0, 9, argc, argv)
	&& !scheme_check_proc_arity(NULL, 1, 9, argc, argv))
      scheme_wrong_contract("make-input-port", "(case-> (-> any)  (any/c . -> . any))", 9, argc, argv);
  }
  name = argv[0];

  /* Shortcut ports for read & peek must be consistent: */
  if (!!read_port != !!peek_port) {
    scheme_contract_error("make-input-port",
                          (read_port
                           ? "read argument is an input port, but peek argument is not a port"
                           : "read argument is not an input port, but peek argument is a port"),
                          "read argument", 1, argv[1],
                          "peek argument", 1, argv[2],
                          NULL);
  }

  /* It makes no sense to supply progress-evt without peek: */
  if ((argc > 5) && SCHEME_FALSEP(argv[2]) && !SCHEME_FALSEP(argv[4]))
    scheme_contract_error("make-input-port",
                          "peek argument is #f, but progress-evt argument is not",
                          "progress evt", 1, argv[4],
                          NULL);
  
  /* It makes no sense to supply peeked-read without progress-evt: */
  if ((argc > 5) && SCHEME_FALSEP(argv[4]) && !SCHEME_FALSEP(argv[5]))
    scheme_contract_error("make-input-port",
                          "progress-evt argument is #f, but commit argument is not",
                          "commit", 1, argv[6],
                          NULL);
  /* Vice-versa: */
  if ((argc > 4) && !SCHEME_FALSEP(argv[4]) && ((argc < 6) || SCHEME_FALSEP(argv[5])))
    scheme_contract_error("make-input-port",
                          "commit argument is #f, but progress-evt argument is not",
                          "progress evt", 1, argv[4],
                          NULL);

  uip = MALLOC_ONE_RT(User_Input_Port);
#ifdef MZTAG_REQUIRED
  uip->type = scheme_rt_user_input;
#endif

  uip->read_proc = argv[1];
  uip->peek_proc = argv[2];
  if (SCHEME_FALSEP(uip->peek_proc))
    uip->peek_proc = NULL;
  uip->close_proc = argv[3];
  uip->progress_evt_proc = ((argc > 4) ? argv[4] : scheme_false);
  if (SCHEME_FALSEP(uip->progress_evt_proc))
    uip->progress_evt_proc = NULL;
  uip->peeked_read_proc = ((argc > 5) ? argv[5] : scheme_false);
  if (SCHEME_FALSEP(uip->peeked_read_proc))
    uip->peeked_read_proc = NULL;
  uip->location_proc = ((argc > 6) ? argv[6] : scheme_false);
  if (SCHEME_FALSEP(uip->location_proc))
    uip->location_proc = NULL;
  if (argc > 7)
    uip->count_lines_proc = argv[7];
  uip->buffer_mode_proc = ((argc > 9) ? argv[9] : scheme_false);
  if (SCHEME_FALSEP(uip->buffer_mode_proc))
    uip->buffer_mode_proc = NULL;

  ip = scheme_make_input_port(scheme_user_input_port_type,
			      uip,
			      name,
			      user_get_bytes,
			      uip->peek_proc ? user_peek_bytes : NULL,
			      uip->progress_evt_proc ? user_progress_evt : NULL,
			      uip->peeked_read_proc ? user_peeked_read : NULL,
			      user_byte_ready,
			      user_close_input,
			      user_needs_wakeup_input,
			      0);

  if (uip->location_proc)
    scheme_set_port_location_fun((Scheme_Port *)ip, user_input_location);
  if (uip->count_lines_proc)
    scheme_set_port_count_lines_fun((Scheme_Port *)ip, user_input_count_lines);

  if (!uip->peek_proc)
    ip->pending_eof = 1; /* means that pending EOFs should be tracked */

  if (argc > 8) {
    if (SCHEME_INTP(argv[8]))
      ip->p.position = (SCHEME_INT_VAL(argv[8]) - 1);
    else if (SCHEME_FALSEP(argv[8]) || SCHEME_BIGNUMP(argv[8]))
      ip->p.position = -1;
    else {
      ip->p.position = 0;
      ip->p.position_redirect = argv[8];
    }
  }

  if (uip->buffer_mode_proc)
    ip->p.buffer_mode_fun = user_input_buffer_mode;

  if (ip->p.count_lines && uip->count_lines_proc)
    scheme_apply_multi(uip->count_lines_proc, 0, NULL);

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_output_port (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  User_Output_Port *uop;
  Scheme_Object *name;

  if (!scheme_is_evt(argv[1])) {
    scheme_wrong_contract("make-output-port", "evt?", 1, argc, argv);
  }
  if (!SCHEME_OUTPUT_PORTP(argv[2])
      && !scheme_check_proc_arity(NULL, 5, 2, argc, argv)) { /* write */
    scheme_wrong_contract("make-output-port", 
                          "(or/c (procedure-arity-includes/c 5) output-port?)", 
                          2, argc, argv);
  }
  scheme_check_proc_arity("make-output-port", 0, 3, argc, argv); /* close */
  if (argc > 4) {
    if (!SCHEME_FALSEP(argv[4])
        && !SCHEME_OUTPUT_PORTP(argv[2])
        && !scheme_check_proc_arity(NULL, 3, 4, argc, argv)) { /* write-special */
      scheme_wrong_contract("make-output-port", 
                            "(or/c (procedure-arity-includes/c 3) output-port?)", 
                            4, argc, argv);
    }
  }
  if (argc > 5)
  scheme_check_proc_arity2("make-output-port", 3, 5, argc, argv, 1); /* write-evt */
  if (argc > 6)
    scheme_check_proc_arity2("make-output-port", 1, 6, argc, argv, 1); /* write-special-evt */
  if (argc > 7)
    scheme_check_proc_arity2("make-output-port", 0, 7, argc, argv, 1); /* get-location */
  if (argc > 8)
    scheme_check_proc_arity("make-output-port", 0, 8, argc, argv); /* count-lines! */
  if (argc > 9) {
    if (!((SCHEME_INTP(argv[9]) && SCHEME_INT_VAL(argv[9]) > 0)
	  || (SCHEME_BIGNUMP(argv[9]) && SCHEME_BIGPOS(argv[9]))
          || SCHEME_FALSEP(argv[9])
          || scheme_check_proc_arity(NULL, 0, 9, argc, argv)
          || SCHEME_INPUT_PORTP(argv[9])
          || SCHEME_OUTPUT_PORTP(argv[9])))
      scheme_wrong_contract("make-output-port", "(or/c exact-positive-integer? port? #f (-> (or/c exact-positive-integer? #f)))", 9, argc, argv);
  }
  if (argc > 10) { /* buffer-mode */
    if (SCHEME_TRUEP(argv[10])
	&& !scheme_check_proc_arity(NULL, 0, 10, argc, argv)
	&& !scheme_check_proc_arity(NULL, 1, 10, argc, argv))
      scheme_wrong_contract("make-output-port", "(case-> (-> any)  (any/c . -> . any))", 10, argc, argv);
  }  

  /* It makes no sense to supply write-special-evt without write-special: */
  if ((argc > 6) && SCHEME_FALSEP(argv[4]) && !SCHEME_FALSEP(argv[6]))
    scheme_contract_error("make-output-port",
                          "write-special argument is #f, but write-special-evt argument is not",
                          "write-special evt", 1, argv[6],
                          NULL);

  /* It makes no sense to supply write-special-evt without write-evt: */
  if ((argc > 6) && SCHEME_FALSEP(argv[5]) && !SCHEME_FALSEP(argv[6]))
    scheme_contract_error("make-output-port",
                          "write-evt argument is #f, but write-special-evt argument is not",
                          "write-special evt", 1, argv[6],
                          NULL);

  /* It makes no sense to supply write-evt without write-special-evt when write-special
     is provided */
  if ((argc > 5) && !SCHEME_FALSEP(argv[5])
      && ((argc < 7) || SCHEME_FALSEP(argv[6]))
      && !SCHEME_FALSEP(argv[4]))
    scheme_contract_error("make-output-port",
                          "write-special-evt argument is #f, but write-evt argument is not, and write-special argument is not",
                          "write evt", 1, argv[4],
                          "write-special evt", 1, argv[6],
                          NULL);
  name = argv[0];

  uop = MALLOC_ONE_RT(User_Output_Port);
#ifdef MZTAG_REQUIRED
  uop->type = scheme_rt_user_output;
#endif

  uop->evt = argv[1];
  uop->write_proc = argv[2];
  uop->close_proc = argv[3];
  uop->write_evt_proc = ((argc > 5) ? argv[5] : scheme_false);
  if (SCHEME_FALSEP(uop->write_evt_proc))
      uop->write_evt_proc = NULL;
  if ((argc < 5) || SCHEME_FALSEP(argv[4])) {
    uop->write_special_proc = NULL;
    uop->write_special_evt_proc = NULL;
  } else {
    uop->write_special_proc = argv[4];
    uop->write_special_evt_proc = ((argc > 6) ? argv[6] : scheme_false);
    if (SCHEME_FALSEP(uop->write_special_evt_proc))
      uop->write_special_evt_proc = NULL;
  }
  if ((argc > 7) && SCHEME_TRUEP(argv[7]))
    uop->location_proc = argv[7];
  if (argc > 8)
    uop->count_lines_proc = argv[8];
  if ((argc > 10) && SCHEME_TRUEP(argv[10]))
    uop->buffer_mode_proc = argv[10];

  op = scheme_make_output_port(scheme_user_output_port_type,
			       uop,
			       name,
			       uop->write_evt_proc ? user_write_bytes_evt : NULL,
			       user_write_bytes,
			       user_write_ready,
			       user_close_output,
			       user_needs_wakeup_output,
			       uop->write_special_evt_proc ? user_write_special_evt : NULL,
			       uop->write_special_proc ? user_write_special : NULL,
			       0);

  if (uop->location_proc)
    scheme_set_port_location_fun((Scheme_Port *)op, user_output_location);
  if (uop->count_lines_proc)
    scheme_set_port_count_lines_fun((Scheme_Port *)op, user_output_count_lines);
  
  if (argc > 9) {
    if (SCHEME_INTP(argv[9]))
      op->p.position = (SCHEME_INT_VAL(argv[9]) - 1);
    else if (SCHEME_FALSEP(argv[9]) && !SCHEME_BIGNUMP(argv[9]))
      op->p.position = -1;
    else {
      op->p.position = 0;
      op->p.position_redirect = argv[9];
    }
  }

  if (uop->buffer_mode_proc)
    op->p.buffer_mode_fun = user_output_buffer_mode;

  if (op->p.count_lines && uop->count_lines_proc)
    scheme_apply_multi(uop->count_lines_proc, 0, NULL);

  return (Scheme_Object *)op;
}

static Scheme_Object *
open_input_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_input_file("open-input-file", 0, argc, argv, 0, NULL, NULL, 0);
}

static Scheme_Object *
open_input_byte_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;
  intptr_t len;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("open-input-bytes", "bytes?", 0, argc, argv);

  len = SCHEME_BYTE_STRTAG_VAL(argv[0]);
  if (SCHEME_IMMUTABLEP(argv[0]))
    len = -len; /* negative means that the string doesn't have to be copied */

  o = scheme_make_sized_byte_string_input_port(SCHEME_BYTE_STR_VAL(argv[0]),
					       len);
  if (argc > 1)
    ((Scheme_Input_Port *)o)->name = argv[1];

  return o;
}

static Scheme_Object *
open_input_char_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("open-input-string", "string?", 0, argc, argv);

  o = scheme_char_string_to_byte_string(argv[0]);

  o = scheme_make_sized_byte_string_input_port(SCHEME_BYTE_STR_VAL(o),
					       -SCHEME_BYTE_STRTAG_VAL(o));

  if (argc > 1)
    ((Scheme_Input_Port *)o)->name = argv[1];

  return o;
}

static Scheme_Object *
open_output_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_output_file("open-output-file", 0, argc, argv, 0, 0, NULL, NULL);
}

static Scheme_Object *
open_input_output_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_output_file("open-input-output-file", 0, argc, argv, 1, 0, NULL, NULL);
}

static Scheme_Object *
open_output_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  o = scheme_make_byte_string_output_port();

  if (argc)
    ((Scheme_Output_Port *)o)->name = argv[0];

  return o;
}

Scheme_Object *do_get_output_string(const char *who, int is_byte,
				    int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  char *s;
  intptr_t size, startpos, endpos;

  op = scheme_output_port_record(argv[0]);
  if (!SCHEME_OUTPUT_PORTP(argv[0])
      || (op->sub_type != scheme_string_output_port_type))
    scheme_wrong_contract(who, "(and/c output-port? string-port?)", 0, argc, argv);

  if (argc > 2) {
    intptr_t len;
    Scheme_Indexed_String *is;

    is = (Scheme_Indexed_String *)op->port_data;
    len = is->index;
    if (is->u.hot > len)
      len = is->u.hot;
    
    startpos = scheme_extract_index(who, 2, argc, argv, len+1, 0);
    if (argc > 3) {
      if (SCHEME_FALSEP(argv[3]))
        endpos = len;
      else {
        endpos = scheme_extract_index(who, 3, argc, argv, len+1, 1);
        if (endpos < 0) 
          endpos = len+1;
      }

      if (!(startpos <= len)) {
        scheme_out_of_range(who, "port", "starting ", argv[2], argv[0], 0, len);
        return NULL;
      }
      if (!(endpos >= startpos && endpos <= len)) {
        scheme_out_of_range(who, "port", "ending ", argv[3], argv[0], startpos, len);
        return NULL;
      }
    } else {
      if (!(startpos <= len)) {
        scheme_out_of_range(who, "port", "starting ", argv[2], argv[0], 0, len);
        return NULL;
      }
      endpos = -1;
    }
  } else {
    startpos = 0;
    endpos = -1;
  }

  s = scheme_get_reset_sized_byte_string_output(argv[0], &size, 
                                                ((argc > 1) && SCHEME_TRUEP(argv[1])), 
                                                startpos, endpos);

  if (is_byte)
    return scheme_make_sized_byte_string(s, size, 0);
  else
    return scheme_make_sized_utf8_string(s, size);
}

static Scheme_Object *
get_output_byte_string (int argc, Scheme_Object *argv[])
{
  return do_get_output_string("get-output-bytes", 1, argc, argv);
}

static Scheme_Object *
get_output_char_string (int argc, Scheme_Object *argv[])
{
  return do_get_output_string("get-output-string", 0, argc, argv);
}

static Scheme_Object *
string_port_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p = argv[0];

  if (SCHEME_INPUT_PORTP(p)) {
    if (SAME_OBJ(scheme_input_port_record(p)->sub_type,
                 scheme_string_input_port_type))
      return scheme_true;
  } else if (SCHEME_OUTPUT_PORTP(p)) {
    if (SAME_OBJ(scheme_output_port_record(p)->sub_type,
                 scheme_string_output_port_type))
      return scheme_true;
  } else {
    scheme_wrong_contract("string-port?", "port?", 0, argc, argv);
  }

  return scheme_false;
}

static Scheme_Object *
close_input_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("close-input-port", "input-port?", 0, argc, argv);

  scheme_close_input_port(argv[0]);
  return (scheme_void);
}

static Scheme_Object *
close_output_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("close-output-port", "output-port?", 0, argc, argv);

  scheme_close_output_port(argv[0]);
  return (scheme_void);
}

static Scheme_Object *
call_with_output_file (int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-output-file", 1, 1, argc, argv);

  port = scheme_do_open_output_file("call-with-output-file", 1, argc, argv, 0, 0, NULL, NULL);

  v = _scheme_apply_multi(argv[1], 1, &port);

  m = p->ku.multiple.array;
  if (v == SCHEME_MULTIPLE_VALUES) {
    if (SAME_OBJ(m, p->values_buffer))
      p->values_buffer = NULL;
  }

  scheme_close_output_port(port);

  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *
call_with_input_file(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-input-file", 1, 1, argc, argv);

  port = scheme_do_open_input_file("call-with-input-file", 1, argc, argv, 0, NULL, NULL, 0);

  v = _scheme_apply_multi(argv[1], 1, &port);

  m = p->ku.multiple.array;
  if (v == SCHEME_MULTIPLE_VALUES) {
    if (SAME_OBJ(m, p->values_buffer))
      p->values_buffer = NULL;
  }

  scheme_close_input_port(port);

  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *with_call_thunk(void *d)
{
  return _scheme_apply_multi(SCHEME_CAR((Scheme_Object *)d), 0, NULL);
}

static void with_close_output(void *d)
{
  scheme_close_output_port(SCHEME_CDR((Scheme_Object *)d));
}


static Scheme_Object *
with_output_to_file (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  scheme_check_proc_arity("with-output-to-file", 0, 1, argc, argv);

  port = scheme_do_open_output_file("with-output-to-file", 1, argc, argv, 0, 0, NULL, NULL);

  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_OUTPUT_PORT,
				port);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL,
			  with_call_thunk,
			  with_close_output,
			  NULL,
			  scheme_make_pair(argv[1], port));

  scheme_pop_continuation_frame(&cframe);

  return v;
}

static void with_close_input(void *d)
{
  scheme_close_input_port(SCHEME_CDR((Scheme_Object *)d));
}

static Scheme_Object *
with_input_from_file(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  scheme_check_proc_arity("with-input-from-file", 0, 1, argc, argv);

  port = scheme_do_open_input_file("with-input-from-file", 1, argc, argv, 0, NULL, NULL, 0);

  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_INPUT_PORT,
				port);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL,
			  with_call_thunk,
			  with_close_input,
			  NULL,
			  scheme_make_pair(argv[1], port));

  scheme_pop_continuation_frame(&cframe);

  return v;
}

static Scheme_Object *sch_default_read_handler(void *ignore, int argc, Scheme_Object *argv[])
{
  Scheme_Object *src;

  if (!SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("default-port-read-handler", "input-port?", 0, argc, argv);

  if ((Scheme_Object *)argv[0] == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (argc > 1)
    src = argv[1];
  else
    src = NULL;

  return scheme_internal_read(argv[0], src, -1, 0, 0, 0, -1, NULL, NULL, NULL, NULL);
}

static int extract_recur_args(const char *who, int argc, Scheme_Object **argv, int delta, 
                              Scheme_Object **_readtable, int *_recur_graph)
{
  int pre_char = -1;

  if (argc > delta + 1) {
    if (SCHEME_TRUEP(argv[delta + 1])) {
      if (!SCHEME_CHARP(argv[delta + 1]))
	scheme_wrong_contract(who, "(or/c char? #f)", delta + 1, argc, argv);
      pre_char = SCHEME_CHAR_VAL(argv[delta + 1]);
    }
    if (argc > delta + 2) {
      Scheme_Object *readtable;
      readtable = argv[delta + 2];
      if (SCHEME_TRUEP(readtable) && !SAME_TYPE(scheme_readtable_type, SCHEME_TYPE(readtable))) {
	scheme_wrong_contract(who, "(or/c readtable? #f)", delta + 2, argc, argv);
      }
      *_readtable = readtable;
      if (argc > delta + 3) {
        *_recur_graph = SCHEME_TRUEP(argv[delta + 3]);
      }
    }
  }

  return pre_char;
}

static Scheme_Object *do_read_f(const char *who, int argc, Scheme_Object *argv[], int recur)
{
  Scheme_Object *port, *readtable = NULL;
  int pre_char = -1, recur_graph = recur;
  Scheme_Input_Port *ip;

  if (argc && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract(who, "input-port?", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (recur) {
    pre_char = extract_recur_args(who, argc, argv, 0, &readtable, &recur_graph);
  }

  ip = scheme_input_port_record(port);

  if (ip->read_handler && !recur) {
    Scheme_Object *o[1];
    o[0] = port;
    return _scheme_apply(ip->read_handler, 1, o);
  } else {
    if (port == scheme_orig_stdin_port)
      scheme_flush_orig_outputs();

    return scheme_internal_read(port, NULL, -1, 0,
                                recur_graph, recur, 
                                pre_char, readtable, 
                                NULL, NULL, NULL);
  }
}

static Scheme_Object *read_f(int argc, Scheme_Object *argv[])
{
  return do_read_f("read", argc, argv, 0);
}

static Scheme_Object *read_recur_f(int argc, Scheme_Object *argv[])
{
  return do_read_f("read/recursive", argc, argv, 1);
}

static Scheme_Object *do_read_syntax_f(const char *who, int argc, Scheme_Object *argv[], int recur)
{
  Scheme_Object *port, *readtable = NULL;
  int pre_char = -1, recur_graph = recur;
  Scheme_Input_Port *ip;

  if ((argc > 1) && !SCHEME_INPUT_PORTP(argv[1]))
    scheme_wrong_contract(who, "input-port?", 1, argc, argv);

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (recur) {
    pre_char = extract_recur_args(who, argc, argv, 1, &readtable, &recur_graph);
  }
  
  ip = scheme_input_port_record(port);

  if (ip->read_handler && !recur) {
    Scheme_Object *o[2], *result;
    o[0] = port;
    o[1] = (argc ? argv[0] : ip->name);

    result = _scheme_apply(ip->read_handler, 2, o);
    if (SCHEME_STXP(result) || SCHEME_EOFP(result))
      return result;
    else {
      o[0] = result;
      /* -1 for argument count indicates "result" */
      scheme_wrong_contract("read handler for read-syntax", "syntax?", 0, -1, o);
      return NULL;
    }
  } else {
    Scheme_Object *src;

    src = (argc ? argv[0] : ip->name);

    if (port == scheme_orig_stdin_port)
      scheme_flush_orig_outputs();

    return scheme_internal_read(port, src, -1, 0,
                                recur, recur_graph,
                                pre_char, readtable, 
                                NULL, NULL, NULL);
  }
}

static Scheme_Object *read_syntax_f(int argc, Scheme_Object *argv[])
{
  return do_read_syntax_f("read-syntax", argc, argv, 0);
}

static Scheme_Object *read_syntax_recur_f(int argc, Scheme_Object *argv[])
{
  return do_read_syntax_f("read-syntax/recursive", argc, argv, 1);
}

static Scheme_Object *read_language(int argc, Scheme_Object **argv)
{
  Scheme_Object *port, *v, *fail_thunk = NULL;

  if (argc > 0) {
    port = argv[0];
    if (!SCHEME_INPUT_PORTP(port))
      scheme_wrong_contract("read-language", "input-port?", 0, argc, argv);
    if (argc > 1) {
      scheme_check_proc_arity("read-language", 0, 1, argc, argv);
      fail_thunk = argv[1];
    }
  } else {
    port = CURRENT_INPUT_PORT(scheme_current_config());
  }
  
  v = scheme_read_language(port, !!fail_thunk);

  if (SCHEME_VOIDP(v))
    return _scheme_tail_apply(fail_thunk, 0, NULL);
  
  return v;
}

static Scheme_Object *
do_read_char(char *name, int argc, Scheme_Object *argv[], int peek, int spec, int is_byte)
{
  Scheme_Object *port;
  int ch;

  if (argc && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract(name, "input-port?", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (peek) {
    Scheme_Object *skip, *unless_evt = NULL;

    if (argc > 1) {
      skip = argv[1];
      if (!(SCHEME_INTP(skip) && (SCHEME_INT_VAL(skip) >= 0))
	  && !(SCHEME_BIGNUMP(skip) && SCHEME_BIGPOS(skip))) {
	scheme_wrong_contract(name, "exact-nonnegative-integer?", 1, argc, argv);
	return NULL;
      }
      if (argc > 2) {
	if (SCHEME_TRUEP(argv[2])) {
	  unless_evt = argv[2];
	  if (!SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type)) {
	    scheme_wrong_contract(name, "progress-evt?", 2, argc, argv);
	    return NULL;
	  }
	  if (!SAME_OBJ(port, SCHEME_PTR1_VAL(unless_evt))) {
	    scheme_contract_error(name,
                                  "evt is not a progress evt for the given port",
                                  "evt", 1, unless_evt,
                                  "port", 1, port,
                                  NULL);
	    return NULL;
	  }
	}
      }
    } else
      skip = NULL;

    if (spec) {
      if (is_byte) {
	ch = scheme_peek_byte_special_ok_skip(port, skip, unless_evt);
      } else
	ch = scheme_peekc_special_ok_skip(port, skip);
    } else {
      if (is_byte)
	ch = scheme_peek_byte_skip(port, skip, unless_evt);
      else
	ch = scheme_peekc_skip(port, skip);
    }
  } else {
    if (spec) {
      if (is_byte)
	ch = scheme_get_byte_special_ok(port);
      else
	ch = scheme_getc_special_ok(port);
    } else {
      if (is_byte)
	ch = scheme_get_byte(port);
      else
	ch = scheme_getc(port);
    }
  }

  if (ch == SCHEME_SPECIAL) {
    return scheme_get_ready_special(port, NULL, peek);
  } else if (ch == EOF)
    return scheme_eof;
  else if (is_byte)
    return scheme_make_integer(ch);
  else
    return _scheme_make_char(ch);
}

static Scheme_Object *
read_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char", argc, argv, 0, 0, 0);
}

static Scheme_Object *
read_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char-or-special", argc, argv, 0, 1, 0);
}

static Scheme_Object *
peek_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char", argc, argv, 1, 0, 0);
}

static Scheme_Object *
peek_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char-or-special", argc, argv, 1, 1, 0);
}

static Scheme_Object *
read_byte (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-byte", argc, argv, 0, 0, 1);
}

static Scheme_Object *
read_byte_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-byte-or-special", argc, argv, 0, 1, 1);
}

static Scheme_Object *
peek_byte (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-byte", argc, argv, 1, 0, 1);
}

static Scheme_Object *
peek_byte_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-byte-or-special", argc, argv, 1, 1, 1);
}

static Scheme_Object *
do_read_line (int as_bytes, const char *who, int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int ch, ascii;
  int crlf = 0, cr = 0, lf = 1;
  char *buf, *oldbuf, onstack[32];
  intptr_t size = 31, oldsize, i = 0;
  Scheme_Input_Port *ip;
  Scheme_Get_String_Fun gs;

  if (argc && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract(who, "input-port?", 0, argc, argv);

  if (argc > 1) {
    Scheme_Object *v = argv[1];
    if (SAME_OBJ(v, any_symbol)) {
      crlf = cr = lf = 1;
    } else if (SAME_OBJ(v, any_one_symbol)) {
      crlf = 0;
      cr = lf = 1;
    } else if (SAME_OBJ(v, cr_symbol)) {
      crlf = lf = 0;
      cr = 1;
    } else if (SAME_OBJ(v, lf_symbol)) {
      crlf = cr = 0;
      lf = 1;
    } else if (SAME_OBJ(v, crlf_symbol)) {
      lf = cr = 0;
      crlf = 1;
    } else
      scheme_wrong_contract(who, "(or/c 'any 'cr 'lf 'crlf)", 1, argc, argv);
  }

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  buf = onstack;

  ip = scheme_input_port_record(port);
  gs = ip->get_string_fun;
  ascii = 1;
      
  while (1) {
    if (!ip->slow) {
      /* `read-line' seems important enough to inline the `read-byte' fast path: */
      char s[1];
      
      ch = gs(ip, s, 0, 1, 0, NULL);

      if (ch == SCHEME_SPECIAL) {
        scheme_bad_time_for_special(who, port);
      } else if (ch) {
        if (ip->p.position >= 0)
          ip->p.position++;

        if (ch != EOF)
          ch = ((unsigned char *)s)[0];
      } else
        ch = scheme_get_byte(port);
    } else {
      ch = scheme_get_byte(port);
    }

    if (ch == EOF) {
      if (!i)
	return scheme_eof;
      break;
    }
    if (ch == '\r') {
      if (crlf) {
	int ch2;

	ch2 = scheme_peek_byte_skip(port, scheme_make_integer(0), NULL);
	if (ch2 == '\n') {
	  scheme_get_byte(port);
	  break;
	} else if (cr)
	  break;
      } else {
	if (cr)
	  break;
      }
    } else if (ch == '\n') {
      if (lf)
	break;
    }

    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (char *)scheme_malloc_atomic(size + 1);
      memcpy(buf, oldbuf, oldsize);
    }
    buf[i++] = ch;
    if (ch > 127) ascii = 0;

    SCHEME_USE_FUEL(1);
  }

  if (as_bytes) {
    buf[i] = '\0';
    return scheme_make_sized_byte_string(buf, i, buf == (char *)onstack);
  } else {
    int j;
    if (ascii) {
      mzchar *us;
      us = scheme_malloc_atomic(sizeof(mzchar) * (i + 1));
      for (j = 0; j < i; j++) {
        us[j] = ((unsigned char *)buf)[j];
      }
      us[i] = 0;
      return scheme_make_sized_offset_char_string(us, 0, i, 0);
    } else {
      buf[i] = '\0';
      return scheme_make_sized_utf8_string(buf, i);
    }
  }
}

static Scheme_Object *
read_line (int argc, Scheme_Object *argv[])
{
  return do_read_line(0, "read-line", argc, argv);
}

static Scheme_Object *
read_byte_line (int argc, Scheme_Object *argv[])
{
  return do_read_line(1, "read-byte-line", argc, argv);
}


static Scheme_Object *
do_general_read_bytes(int as_bytes,
		      const char *who, int argc, Scheme_Object *argv[],
		      int alloc_mode, int only_avail, int peek)
{
  Scheme_Object *port, *str, *peek_skip, *unless_evt = NULL;
  intptr_t size, start, finish, got;
  int delta, size_too_big = 0;

  if (alloc_mode) {
    if (!SCHEME_INTP(argv[0])) {
      if (SCHEME_BIGNUMP(argv[0])) {
	size = 1;
	size_too_big = 1;
      } else
	size = -1; /* cause the error message to be printed */
    } else
      size = SCHEME_INT_VAL(argv[0]);

    if (size < 0) {
      scheme_wrong_contract(who, "exact-nonnegative-integer?", 0, argc, argv);
      return NULL;
    }
    str = NULL; /* allocated later */
  } else {
    if (as_bytes) {
      if (!SCHEME_MUTABLE_BYTE_STRINGP(argv[0])) {
	scheme_wrong_contract(who, "(and/c bytes? (not/c immutable?))", 0, argc, argv);
	return NULL;
      }
    } else {
      if (!SCHEME_MUTABLE_CHAR_STRINGP(argv[0])) {
	scheme_wrong_contract(who, "(and/c strings? (not/c immutable?))", 0, argc, argv);
	return NULL;
      }
    }
    str = argv[0];
    size = 0;
  }

  if (peek) {
    Scheme_Object *v;
    v = argv[1];
    if (SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 0))
      peek_skip = v;
    else if (SCHEME_BIGNUMP(v) && SCHEME_BIGPOS(v))
      peek_skip = v;
    else {
      scheme_wrong_contract(who, "exact-nonnegative-integer?", 1, argc, argv);
      return NULL;
    }
    if (only_avail) {
      if (SCHEME_TRUEP(argv[2])) {
	unless_evt = argv[2];
	if (!SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type)) {
	  scheme_wrong_contract(who, "(or/c progress-evt? #f)", 2, argc, argv);
	  return NULL;
	}
      }
      delta = 2;
    } else
      delta = 1;
  } else {
    peek_skip = scheme_make_integer(0);
    delta = 0;
  }

  if ((argc > (1+delta)) && !SCHEME_INPUT_PORTP(argv[1+delta]))
    scheme_wrong_contract(who, "input-port?", 1+delta, argc, argv);

  if (alloc_mode) {
    start = 0;
    finish = size;
  } else {
    scheme_get_substring_indices(who, str,
				 argc, argv,
				 2+delta, 3+delta, &start, &finish);

    size = finish - start;
  }

  if (argc > (delta+1))
    port = argv[delta+1];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (unless_evt) {
    if (!SAME_OBJ(port, SCHEME_PTR1_VAL(unless_evt))) {
      scheme_contract_error(who,
                            "evt is not a progress evt for the given port",
                            "evt", 1, unless_evt,
                            "port", 1, port,
                            NULL);
      return NULL;
    }
  }

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (!size) {
    if (alloc_mode) {
      if (as_bytes)
	return scheme_make_sized_byte_string("", 0, 0);
      else
	return scheme_make_sized_char_string((mzchar *)"\0\0\0", 0, 0);
    } else
      return scheme_make_integer(0);
  }

  if (alloc_mode) {
    if (size_too_big) {
      scheme_raise_out_of_memory(who, "making string of length %s",
				 scheme_make_provided_string(argv[0], 0, NULL));
      return NULL;
    }
    if (as_bytes)
      str = scheme_alloc_byte_string(size, 0);
    else
      str = scheme_alloc_char_string(size, 0);
  }

  if (as_bytes) {
    got = scheme_get_byte_string_special_ok_unless(who, port,
						   SCHEME_BYTE_STR_VAL(str), start, size,
						   only_avail,
						   peek, peek_skip,
						   unless_evt);
    if (got == SCHEME_SPECIAL) {
      Scheme_Object *res;
      res = scheme_get_special_proc(port);
      if (!only_avail)
	scheme_bad_time_for_special(who, port);
      return res;
    }
  } else {
    got = scheme_get_char_string(who, port,
				 SCHEME_CHAR_STR_VAL(str), start, size,
				 peek, peek_skip);
  }

  if (got == EOF)
    return scheme_eof;

  if (alloc_mode) {
    if (got < size) {
      /* Ended up with a shorter string: */
      if (as_bytes)
	str = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(str), got, 1);
      else
	str = scheme_make_sized_char_string(SCHEME_CHAR_STR_VAL(str), got, 1);
    }
    return str;
  } else
    return scheme_make_integer(got);
}

static Scheme_Object *
sch_read_bytes(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes", argc, argv, 1, 0, 0);
}

static Scheme_Object *
sch_read_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes!", argc, argv, 0, 0, 0);
}

static Scheme_Object *
sch_peek_bytes(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes", argc, argv, 1, 0, 1);
}

static Scheme_Object *
sch_peek_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes!", argc, argv, 0, 0, 1);
}

static Scheme_Object *
read_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!", argc, argv, 0, 1, 0);
}

static Scheme_Object *
read_bytes_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!*", argc, argv, 0, 2, 0);
}

static Scheme_Object *
peeked_read(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *unless_evt, *target_evt;
  intptr_t size;
  int v;

  if ((SCHEME_INTP(argv[0]) && (SCHEME_INT_VAL(argv[0]) > 0))
      || (SCHEME_BIGNUMP(argv[0]) && SCHEME_BIGPOS(argv[0]))) {
    if (SCHEME_INTP(argv[0]))
      size = SCHEME_INT_VAL(argv[0]);
    else
      size = 0x7FFFFFFF;
  } else {
    scheme_wrong_contract("port-commit-peeked", "exact-positive-integer?", 0, argc, argv);
    return NULL;
  }

  unless_evt = argv[1];
  target_evt = argv[2];
  if (!SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type))
    scheme_wrong_contract("port-commit-peeked", "progress-evt?", 1, argc, argv);
  {
    Scheme_Type t;
    t = SCHEME_TYPE(target_evt);
    if (!SAME_TYPE(t, scheme_sema_type)
	&& !SAME_TYPE(t, scheme_channel_type)
	&& !SAME_TYPE(t, scheme_channel_put_type)
	&& !SAME_TYPE(t, scheme_always_evt_type)
	&& !SAME_TYPE(t, scheme_never_evt_type)
	&& !SAME_TYPE(t, scheme_semaphore_repost_type))
      scheme_wrong_contract("port-commit-peeked", 
                            "(or/c channel-put-evt? channel? semaphore? semephore-peek-evt? (one-of/c always-evt never evt))",
                            2, argc, argv);
  }

  if (argc > 3) {
    port = argv[3];
    if (!SCHEME_INPUT_PORTP(port))
      scheme_wrong_contract("port-commit-peeked", "input-port?", 3, argc, argv);
  } else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (!SAME_OBJ(port, SCHEME_PTR1_VAL(unless_evt))) {
    scheme_contract_error("port-commit-peeked",
                          "evt is not a progress evt for the given port",
                          "evt", 1, unless_evt,
                          "port", 1, port,
                          NULL);
    return NULL;
  }

  v = scheme_peeked_read(port, size, unless_evt, target_evt);

  return (v ? scheme_true : scheme_false);
}

static Scheme_Object *
peek_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!", argc, argv, 0, 1, 1);
}

static Scheme_Object *
peek_bytes_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!*", argc, argv, 0, 2, 1);
}

static Scheme_Object *
sch_read_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "read-string", argc, argv, 1, 0, 0);
}

static Scheme_Object *
sch_read_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "read-string!", argc, argv, 0, 0, 0);
}

static Scheme_Object *
sch_peek_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "peek-string", argc, argv, 1, 0, 1);
}

static Scheme_Object *
sch_peek_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "peek-string!", argc, argv, 0, 0, 1);
}

static Scheme_Object *
read_bytes_bang_break(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!/enable-break", argc, argv, 0, -1, 0);
}

static Scheme_Object *
peek_bytes_bang_break(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!/enable-break", argc, argv, 0, -1, 1);
}

static Scheme_Object *
progress_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  if (argc) {
    if (!SCHEME_INPUT_PORTP(argv[0])) {
      scheme_wrong_contract("port-progress-evt", "input-port?", 0, argc, argv);
      return NULL;
    }
    port = argv[0];
  } else {
    port = CURRENT_INPUT_PORT(scheme_current_config());
  }

  v = scheme_progress_evt(port);

  if (!v) {
    scheme_contract_error("port-progress-evt", "port does not provide progress evts",
                          "port", 1, port,
                          NULL);
    return NULL;
  } else
    return v;
}

static Scheme_Object *
is_progress_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = argv[0];

  if (argc > 1) {
    if (!SAME_TYPE(SCHEME_TYPE(v), scheme_progress_evt_type))
      scheme_wrong_contract("progress-evt?", "progress-evt?", 0, argc, argv);
    if (!SCHEME_INPUT_PORTP(argv[1]))
      scheme_wrong_contract("progress-evt?", "input-port?", 1, argc, argv);
    return (SAME_OBJ(argv[1], SCHEME_PTR1_VAL(v))
            ? scheme_true
            : scheme_false);
  } else {
    return (SAME_TYPE(SCHEME_TYPE(v), scheme_progress_evt_type)
            ? scheme_true
            : scheme_false);
  }
}

static Scheme_Object *make_closed_evt(int already_closed)
{
  Scheme_Object *evt, *sema;

  sema = scheme_make_sema(0);
  if (already_closed)
    scheme_post_sema_all(sema);
  evt = scheme_alloc_small_object();
  evt->type = scheme_port_closed_evt_type;
  SCHEME_PTR_VAL(evt) = sema;

  return evt;
}

static Scheme_Object *
closed_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];
  if (SCHEME_INPUT_PORTP(v)) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(v);
    if (!ip->closed_evt) {
      v = make_closed_evt(ip->closed);
      ip->closed_evt = v;
    } else
      v = ip->closed_evt;
    return v;
  } else if (SCHEME_OUTPUT_PORTP(v)) {
    Scheme_Output_Port *op;
    op = scheme_output_port_record(v);
    if (!op->closed_evt) {
      v = make_closed_evt(op->closed);
      op->closed_evt = v;
    } else
      v = op->closed_evt;
    return v;
  } else {
    scheme_wrong_contract("port-closed-evt", "port?", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *
do_write_bytes_avail(int as_bytes, const char *who,
		     int argc, Scheme_Object *argv[],
		     int rarely_block, int get_evt)
{
  Scheme_Object *port, *str;
  intptr_t size, start, finish, putten;

  if (as_bytes && !SCHEME_BYTE_STRINGP(argv[0])) {
    scheme_wrong_contract(who, "bytes?", 0, argc, argv);
    return NULL;
  } else if (!as_bytes && !SCHEME_CHAR_STRINGP(argv[0])) {
    scheme_wrong_contract(who, "string?", 0, argc, argv);
    return NULL;
  } else
    str = argv[0];
  if ((argc > 1) && !SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract(who, "output-port?", 1, argc, argv);

  scheme_get_substring_indices(who, str,
			       argc, argv,
			       2, 3, &start, &finish);

  size = finish - start;

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  if (get_evt)
    return scheme_make_write_evt(who, port,
				 NULL, SCHEME_BYTE_STR_VAL(str), start, size);
  else if (as_bytes)
    putten = scheme_put_byte_string(who, port,
				    SCHEME_BYTE_STR_VAL(str), start, size,
				    rarely_block);
  else
    putten = scheme_put_char_string(who, port,
				    SCHEME_CHAR_STR_VAL(str), start, size);

  if (putten < 0)
    return scheme_false;
  else
    return scheme_make_integer(putten);
}

static Scheme_Object *
write_bytes(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes", argc, argv, 0, 0);
}

static Scheme_Object *
write_bytes_avail(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail", argc, argv, 1, 0);
}

static Scheme_Object *
write_bytes_avail_break(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail", argc, argv, -1, 0);
}

static Scheme_Object *
write_bytes_avail_nonblock(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail*", argc, argv, 2, 0);
}

static Scheme_Object *
write_string(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(0, "write-string", argc, argv, 0, 0);
}

static Scheme_Object *
write_bytes_avail_evt(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail-evt", argc, argv, 1, 1);
}

static Scheme_Object *
do_write_special(const char *name, int argc, Scheme_Object *argv[], int nonblock, int get_evt)
{
  Scheme_Output_Port *op;
  Scheme_Object *port;
  int ok;

  if (argc > 1) {
    if (!SCHEME_OUTPUT_PORTP(argv[1]))
      scheme_wrong_contract(name, "output-port?", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  op = scheme_output_port_record(port);

  if (op->write_special_fun) {
    if (get_evt) {
      return scheme_make_write_evt(name, port, argv[0], NULL, 0, 0);
    } else {
      Scheme_Write_Special_Fun ws = op->write_special_fun;
      ok = ws(op, argv[0], nonblock);
    }
  } else {
    scheme_contract_error(name,
                          "port does not support special values",
                          "port", 1, port,
                          NULL);
    return NULL;
  }

  if (ok) {
    Scheme_Port *ip;
    ip = scheme_port_record(port);
    if (ip->position >= 0)
      ip->position += 1;
    if (ip->count_lines) {
      ip->column += 1;
      ip->readpos += 1;
      ip->charsSinceNewline += 1;
      ip->utf8state = 0;
    }
    return scheme_true;
  } else
    return scheme_false;
}

static Scheme_Object *can_write_atomic(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-writes-atomic?", "output-port?", 0, argc, argv);
  
  op = scheme_output_port_record(argv[0]);
  if (op->write_string_evt_fun)
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *can_provide_progress_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;

  if (!SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-provides-progress-evt?", "input-port?", 0, argc, argv);

  ip = scheme_input_port_record(argv[0]);

  if (ip->progress_evt_fun)
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
can_write_special(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-writes-special?", "output-port?", 0, argc, argv);

  op = scheme_output_port_record(argv[0]);

  if (op->write_special_fun)
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *
scheme_write_special(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special", argc, argv, 0, 0);
}

Scheme_Object *
scheme_write_special_nonblock(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special-avail*", argc, argv, 1, 0);
}

static Scheme_Object *
write_special_evt(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special-evt", argc, argv, 1, 1);
}


Scheme_Object *
scheme_call_enable_break(Scheme_Prim *prim, int argc, Scheme_Object *argv[])
{
  Scheme_Cont_Frame_Data cframe;
  Scheme_Object *v;

  scheme_push_break_enable(&cframe, 1, 1);

  v = prim(argc, argv);

  scheme_pop_break_enable(&cframe, 0);

  return v;
}

static Scheme_Object *
eof_object_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_EOFP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
char_ready_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("char-ready?", "input-port?", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  return (scheme_char_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *
byte_ready_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("byte-ready?", "input-port?", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  return (scheme_byte_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract("default-port-display-handler", "output-port?", 1, argc, argv);

  scheme_internal_display(argv[0], argv[1]);

  return scheme_void;
}

static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract("default-port-write-handler", "output-port?", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1]);

  return scheme_void;
}

static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract("default-port-print-handler", "output-port?", 1, argc, argv);
  if ((argc > 2) && !scheme_nonneg_exact_p(argv[2]))
    scheme_wrong_contract("default-port-print-handler", "exact-nonnegative-integer?", 
                          2, argc, argv);
  
  return _scheme_apply(scheme_get_param(scheme_current_config(),
					MZCONFIG_PORT_PRINT_HANDLER),
		       argc, argv);
}

static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[1]))
    scheme_wrong_contract("default-global-port-print-handler", "output-port?", 1, argc, argv);
  if ((argc > 2) 
      && !SAME_OBJ(argv[2], scheme_make_integer(0))
      && !SAME_OBJ(argv[2], scheme_make_integer(1)))
    scheme_wrong_contract("default-global-port-print-handler", "(or/c 0 1)", 
                          2, argc, argv);

  if (argc == 2) {
    scheme_internal_print(argv[0], argv[1], scheme_make_integer(0));
  }
  else {
    scheme_internal_print(argv[0], argv[1], argv[2]);
  }

  return scheme_void;
}

static Scheme_Object *
display_write(char *name,
	      int argc, Scheme_Object *argv[], int escape)
{
  Scheme_Object *port;
  Scheme_Output_Port *op;

  if (argc > 1) {
    if (!SCHEME_OUTPUT_PORTP(argv[1]))
      scheme_wrong_contract(name, "output-port?", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  op = scheme_output_port_record(port);

  if (escape > 0) {
    /* display */
    if (!op->display_handler) {
      Scheme_Object *v = argv[0];
      if (SCHEME_BYTE_STRINGP(v)) {
	scheme_put_byte_string(name, port,
			       SCHEME_BYTE_STR_VAL(v), 0, SCHEME_BYTE_STRLEN_VAL(v),
			       0);
      } else if (SCHEME_CHAR_STRINGP(v)) {
	scheme_put_char_string(name, port,
			       SCHEME_CHAR_STR_VAL(v), 0, SCHEME_CHAR_STRLEN_VAL(v));
      } else if (SCHEME_SYMBOLP(v)) {
	scheme_put_byte_string(name, port,
                               (char *)v, ((char *)(SCHEME_SYM_VAL(v))) - ((char *)v), 
                               SCHEME_SYM_LEN(v),
			       0);
      } else 
	scheme_internal_display(v, port);
    } else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(op->display_handler, 2, a);
    }
  } else if (!escape) {
    /* write */
    Scheme_Object *h;

    h = op->write_handler;

    if (!h)
      scheme_internal_write(argv[0], port);
    else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(h, 2, a);
    }
  } else {
    /* print */
    Scheme_Object *h;
    Scheme_Object *a[3];
    
    if (argc > 2) {
      h = argv[2];
      if (!SAME_OBJ(h, scheme_make_integer(0))
          && !SAME_OBJ(h, scheme_make_integer(1)))
        scheme_wrong_contract(name, "(or/c 0 1)", 2, argc, argv);
    } else
      h = scheme_make_integer(0);

    a[0] = argv[0];
    a[1] = (Scheme_Object *)port;
    a[2] = h;

    h = op->print_handler;

    if (!h)
      sch_default_print_handler(3, a);
    else
      _scheme_apply_multi(h, 3, a);
  }

  return scheme_void;
}

static Scheme_Object *
sch_write (int argc, Scheme_Object *argv[])
{
  return display_write("write", argc, argv, 0);
}

static Scheme_Object *
display (int argc, Scheme_Object *argv[])
{
  return display_write("display", argc, argv, 1);
}

static Scheme_Object *
sch_print (int argc, Scheme_Object *argv[])
{
  return display_write("print", argc, argv, -1);
}

static Scheme_Object *
newline (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("newline", "output-port?", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  (void)scheme_put_byte_string("newline", port, "\n", 0, 1, 0);

  return scheme_void;
}

static Scheme_Object *
write_byte (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int v;
  unsigned char buffer[1];

  if (argc && !SCHEME_INTP(argv[0]))
    scheme_wrong_contract("write-byte", "byte?", 0, argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  if ((v < 0) || (v > 255))
    scheme_wrong_contract("write-byte", "byte?", 0, argc, argv);

  if (argc > 1) {
    if (!SCHEME_OUTPUT_PORTP(argv[1]))
      scheme_wrong_contract("write-byte", "output-port?", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  buffer[0] = v;

  scheme_put_byte_string("write-byte", port,
			 (char *)buffer, 0, 1,
			 0);

  return scheme_void;
}

static Scheme_Object *
write_char (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  unsigned char buffer[MAX_UTF8_CHAR_BYTES];
  unsigned int ubuffer[1];
  int len;

  if (argc && !SCHEME_CHARP(argv[0]))
    scheme_wrong_contract("write-char", "char?", 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_OUTPUT_PORTP(argv[1]))
      scheme_wrong_contract("write-char", "output-port?", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  ubuffer[0] = SCHEME_CHAR_VAL(argv[0]);
  len = scheme_utf8_encode_all(ubuffer, 1, buffer);

  scheme_put_byte_string("write-char", port,
			 (char *)buffer, 0, len,
			 0);

  return scheme_void;
}

static Scheme_Object *port_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;

  if (!SCHEME_INPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-read-handler", "input-port?", 0, argc, argv);

  ip = scheme_input_port_record(argv[0]);
  if (argc == 1) {
    if (ip->read_handler)
      return ip->read_handler;
    else
      return default_read_handler;
  } else {
    if (argv[1] == default_read_handler)
      ip->read_handler = NULL;
    else {
      if (!scheme_check_proc_arity(NULL, 1, 1, argc, argv)
	  || !scheme_check_proc_arity(NULL, 2, 1, argc, argv)) {
	scheme_wrong_contract("port-read-handler", 
                              "(case-> (any/c . -> . any)  (any/c any/c . -> . any))", 
                              1, argc, argv);
	return NULL;
      }

      ip->read_handler = argv[1];
    }

    return scheme_void;
  }
}

static Scheme_Object *port_display_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-display-handler", "output-port?", 0, argc, argv);

  op = scheme_output_port_record(argv[0]);
  if (argc == 1) {
    if (op->display_handler)
      return op->display_handler;
    else
      return default_display_handler;
  } else {
    scheme_check_proc_arity("port-display-handler", 2, 1, argc, argv);
    if (argv[1] == default_display_handler)
      op->display_handler = NULL;
    else
      op->display_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_write_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-write-handler", "output-port?", 0, argc, argv);

  op = scheme_output_port_record(argv[0]);
  if (argc == 1) {
    if (op->write_handler)
      return op->write_handler;
    else
      return default_write_handler;
  } else {
    scheme_check_proc_arity("port-write-handler", 2, 1, argc, argv);
    if (argv[1] == default_write_handler)
      op->write_handler = NULL;
    else
      op->write_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *call_print_handler(void *data, int argc, Scheme_Object *argv[])
{
  /* If there's a 3rd argument, drop it. */
  return _scheme_tail_apply((Scheme_Object *)data, 2, argv);
}

static Scheme_Object *wrap_print_handler(Scheme_Object *proc)
{
  return scheme_make_closed_prim_w_arity(call_print_handler,
                                         proc,
                                         "wrapped-port-print-handler",
                                         2, 3);
}

static Scheme_Object *port_print_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-print-handler", "output-port?", 0, argc, argv);

  op = scheme_output_port_record(argv[0]);
  if (argc == 1) {
    if (op->print_handler)
      return op->print_handler;
    else
      return default_print_handler;
  } else {
    scheme_check_proc_arity("port-print-handler", 2, 1, argc, argv);
    if (argv[1] == default_print_handler)
      op->print_handler = NULL;
    else if (!scheme_check_proc_arity(NULL, 3, 1, argc, argv)) {
      Scheme_Object *wrapped;
      wrapped = wrap_print_handler(argv[1]);
      op->print_handler = wrapped;
    } else
      op->print_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *filter_print_handler(int argc, Scheme_Object **argv)
{
  if (scheme_check_proc_arity(NULL, 2, 0, argc, argv)) {
    if (scheme_check_proc_arity(NULL, 3, 0, argc, argv))
      return argv[0];
    else
      return wrap_print_handler(argv[0]);
  } else
    return NULL;
}

static Scheme_Object *global_port_print_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("global-port-print-handler",
                              scheme_make_integer(MZCONFIG_PORT_PRINT_HANDLER),
                              argc, argv,
                              -1, filter_print_handler, "(procedure-arity-includes/c 2)", 1);
}

static Scheme_Object *port_count_lines(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPUT_PORTP(argv[0]) && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-count-lines!", "port?", 0, argc, argv);

  scheme_count_lines(argv[0]);

  return scheme_void;
}

static Scheme_Object *port_counts_lines_p(int argc, Scheme_Object *argv[])
{
  Scheme_Port *ip;

  if (!SCHEME_INPUT_PORTP(argv[0]) && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-counts-lines?", "port?", 0, argc, argv);

  ip = scheme_port_record(argv[0]);

  return (ip->count_lines ? scheme_true : scheme_false);
}

static Scheme_Object *global_port_count_lines(int argc, Scheme_Object **argv)
{
  return scheme_param_config("port-count-lines-enabled",
			     scheme_make_integer(MZCONFIG_PORT_COUNT_LINES),
			     argc, argv, -1, NULL, NULL, 1);
}

static Scheme_Object *port_next_location(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[3];
  intptr_t line, col, pos;

  if (!SCHEME_INPUT_PORTP(argv[0]) && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("port-next-location", "port?", 0, argc, argv);

  scheme_tell_all(argv[0], &line, &col, &pos);

  a[0] = ((line < 0) ? scheme_false : scheme_make_integer_value(line));
  a[1] = ((col < 0) ? scheme_false : scheme_make_integer_value(col));
  a[2] = ((pos < 0) ? scheme_false : scheme_make_integer_value(pos+1));

  return scheme_values(3, a);
}

static Scheme_Object *set_port_next_location(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPUT_PORTP(argv[0]) && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("set-port-next-location!", "port?", 0, argc, argv);

  scheme_set_port_location(argc, argv);
  
  return scheme_void;
}

static Scheme_Object *filesystem_change_evt(int argc, Scheme_Object *argv[])
{
  Scheme_Object *e;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("filesystem-change-evt", "path-string?", 0, argc, argv);
  if (argc > 1)
    scheme_check_proc_arity("filesystem-change-evt", 0, 1, argc, argv);

  e = scheme_filesystem_change_evt(argv[0], 0, (argc < 2));

  if (!e)
    return _scheme_tail_apply(argv[1], 0, NULL);
  else
    return e;
}

static Scheme_Object *filesystem_change_evt_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(scheme_filesystem_change_evt_type, SCHEME_TYPE(argv[0]))
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *filesystem_change_evt_cancel(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(scheme_filesystem_change_evt_type, SCHEME_TYPE(argv[0])))
    scheme_wrong_contract("filesystem-change-evt-cancel", "filesystem-change-evt?", 0, argc, argv);

  scheme_filesystem_change_evt_cancel(argv[0], NULL);

  return scheme_void;
}

static intptr_t get_number(Scheme_Object *port, intptr_t pos)
{
  unsigned char buffer[4];
  intptr_t got, orig;

  orig = scheme_set_file_position(port, -1);
  scheme_set_file_position(port, pos);

  got = scheme_get_byte_string("default-load-handler",
                               port,
                               (char *)buffer, 0, 4,
                               0, 0, scheme_make_integer(0));

  (void)scheme_set_file_position(port, orig);

  if (got != 4)
    return 0;

  return (buffer[0] | (buffer[1] << 8) | (buffer[2] << 16) | (buffer[3] << 24));
}

static char *get_bytes(Scheme_Object *port, intptr_t pos, intptr_t len)
{
  char *s;
  intptr_t orig;

  s = scheme_malloc_atomic(len + 1);
  s[len] = 0;

  orig = scheme_set_file_position(port, -1);
  scheme_set_file_position(port, pos);

  scheme_get_byte_string("default-load-handler",
                         port,
                         (char *)s, 0, len,
                         0, 0, scheme_make_integer(0));

  (void)scheme_set_file_position(port, orig);
  
  return s;
}

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *port;
  Scheme_Thread *p;
  Scheme_Object *stxsrc;
  Scheme_Object *expected_module;
} LoadHandlerData;

static void post_load_handler(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;

  scheme_close_input_port((Scheme_Object *)lhd->port);
}

static Scheme_Object *do_load_handler(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;
  Scheme_Object *port = lhd->port;
  Scheme_Thread *p = lhd->p;
  Scheme_Config *config = lhd->config;
  Scheme_Object *last_val = scheme_void, *obj, **save_array = NULL, *modname;
  Scheme_Env *genv;
  int save_count = 0, got_one = 0, as_module, check_module_name = 0, skip_no_more_check = 0;

  modname = lhd->expected_module;

  if (SCHEME_TRUEP(modname)) {
    /* Look for a module directory: */
    intptr_t got;
    int vers_size, dir_header_size;
#   define DIR_HEADER_SIZE (3 + 20 + 16)
    char buffer[DIR_HEADER_SIZE];

    vers_size = strlen(MZSCHEME_VERSION);
    dir_header_size = 4 + vers_size;
    if (dir_header_size >= DIR_HEADER_SIZE) 
      scheme_signal_error("internal error: buffer size mismatch");
    got = scheme_get_byte_string("default-load-handler",
                                 port,
                                 buffer, 0, dir_header_size,
                                 0, 1, scheme_make_integer(0));

    if ((got == dir_header_size)
        && (buffer[0] == '#')
        && (buffer[1] == '~')
        && (buffer[2] == vers_size)
        && (!scheme_strncmp(buffer + 3, MZSCHEME_VERSION, vers_size))
        && (buffer[3 + vers_size] == 'D')) {    
      /* File starts with a directory. The directory is a balanced binary search tree,
         where each node has the shape 
           <name-len> <name-bytes> <mod-pos> <mod-len> <left-pos> <right-pos>
         and a 0 position for <left-pos> or <right-pos> means no child. */
      char *find_name, *s;
      intptr_t namelen, i, name_size, pos, offset = 0, rellen;

      if (SCHEME_PAIRP(modname))
        find_name = scheme_submodule_path_to_string(SCHEME_CDR(modname), &namelen);
      else {
        find_name = "";
        namelen = 0;
      }

      pos = dir_header_size + 4 /* skip total-module count */;
      
      while (pos) {
        name_size = get_number(port, pos);
        s = get_bytes(port, pos + 4, name_size);
        if ((name_size == namelen) && !strncmp(find_name, s, name_size)) {
          /* found it */
          offset = get_number(port, pos + 4 + name_size);
          break;
        }
        /* try left or right? */
        rellen = namelen;
        for (i = 0; (i < rellen) && (i < name_size); i++) {
          if (find_name[i] != s[i]) {
            if (((unsigned char *)find_name)[i] < ((unsigned char *)s)[i])
              rellen = 0;
            else
              rellen = name_size + 1;
            break;
          }
        }
        if (rellen < name_size)
          pos = get_number(port, pos + 12 + name_size);
        else
          pos = get_number(port, pos + 16 + name_size);
      }

      if (offset) {
        scheme_set_file_position(port, offset);
        if (!SCHEME_SYMBOLP(modname))
          modname = SCHEME_CAR(SCHEME_CDR(modname));
        skip_no_more_check = 1;
      } else if (SCHEME_PAIRP(modname)) {
        /* don't complain if a submodule isn't found */
        return scheme_void;
      }
    }
  } 

  if (SCHEME_PAIRP(modname)) {
    modname = SCHEME_CAR(modname);

    if (SCHEME_FALSEP(modname)) {
      /* caller says the main module is already loaded, 
         so don't reload for submodules */
      return scheme_void;
    }
  }

  if (scheme_module_code_cache && SCHEME_TRUEP(modname)) {
    intptr_t got;
    int vers_size, hash_header_size;
#   define HASH_HEADER_SIZE (4 + 20 + 16)
    char buffer[HASH_HEADER_SIZE];

    vers_size = strlen(MZSCHEME_VERSION);
    hash_header_size = 4 + vers_size + 20;
    if (hash_header_size >= HASH_HEADER_SIZE) 
      scheme_signal_error("internal error: buffer size mismatch");
    got = scheme_get_byte_string("default-load-handler",
                                 port,
                                 buffer, 0, hash_header_size,
                                 0, 1, scheme_make_integer(0));

    obj = NULL;
    if ((got == hash_header_size)
        && (buffer[0] == '#')
        && (buffer[1] == '~')
        && (buffer[2] == vers_size)
        && (!scheme_strncmp(buffer + 3, MZSCHEME_VERSION, vers_size))
        && (buffer[3 + vers_size] == 'T')) {
      int i;
      for (i = 0; i < 20; i++) {
        if (buffer[4 + vers_size + i])
          break;
      }
      if (i < 20) {
        obj = scheme_make_sized_byte_string(buffer + 4 + vers_size, 20, 1);
      }
    }


    if (obj) {
      Scheme_Object *dir;
      dir = scheme_get_param(config, MZCONFIG_LOAD_DIRECTORY);
      if (SCHEME_TRUEP(dir))
        dir = scheme_path_to_directory_path(dir);
      obj = scheme_make_pair(obj, dir);
      obj = scheme_lookup_in_table(scheme_module_code_cache, (const char *)obj);
      if (obj)
        obj = scheme_ephemeron_value(obj);
      if (obj) {
        /* Synthesize a wrapper to pass through `eval': */
        Scheme_Compilation_Top *top;

        top = MALLOC_ONE_TAGGED(Scheme_Compilation_Top);
        top->iso.so.type = scheme_compilation_top_type;
        top->code = obj;
        top->prefix = NULL; /* indicates a wrapper */

        obj = (Scheme_Object *)top;
        
        return _scheme_apply_multi(scheme_get_param(config, MZCONFIG_EVAL_HANDLER),
                                   1, &obj);
      }
    }
  }

  while ((obj = scheme_internal_read(port, lhd->stxsrc, -1, 0, 0, 0, -1, NULL, 
                                     NULL, NULL, NULL))
	 && !SCHEME_EOFP(obj)) {
    save_array = NULL;
    got_one = 1;

    /* ... begin special support for module loading ... */

    genv = scheme_get_env(config);
    as_module = 0;

    if (SCHEME_SYMBOLP(modname)) {
      /* Must be of the form `(module <somename> ...)',possibly compiled. */
      /* Also, file should have no more expressions. */
      Scheme_Object *a, *d, *other = NULL;
      Scheme_Module *m;

      d = obj;

      m = scheme_extract_compiled_module(SCHEME_STX_VAL(d));
      if (m) {
        if (check_module_name) {
          if (!scheme_resolved_module_path_value_matches(m->modname, modname)) {
            other = m->modname;
            d = NULL;
          }
        }
      } else {
	if (!SCHEME_STX_PAIRP(d))
	  d = NULL;
	else {
	  a = SCHEME_STX_CAR(d);
	  if (!SAME_OBJ(SCHEME_STX_VAL(a), module_symbol))
	    d = NULL;
	  else {
	    d = SCHEME_STX_CDR(d);
	    if (!SCHEME_STX_PAIRP(d))
	      d = NULL;
	    else {
	      a = SCHEME_STX_CAR(d);
	      other = SCHEME_STX_VAL(a);
              if (check_module_name) {
                if (!SAME_OBJ(other, modname))
                  d = NULL;
              }
	    }
	  }
	}
      }

      /* If d is NULL, shape was wrong */
      if (!d) {
        Scheme_Object *err_msg;
	if (!other || !SCHEME_SYMBOLP(other))
	  err_msg = scheme_make_byte_string("something else");
	else {
	  char *s, *t;
	  intptr_t len, slen;

	  t = "declaration for `";
	  len = strlen(t);
	  slen = SCHEME_SYM_LEN(other);

	  s = (char *)scheme_malloc_atomic(len + slen + 2);
	  memcpy(s, t, len);
	  memcpy(s + len, SCHEME_SYM_VAL(other), slen);
	  s[len + slen] = '\'';
	  s[len + slen + 1]= 0;

	  err_msg = scheme_make_sized_byte_string(s, len + slen + 1, 0);
	}

        {
          Scheme_Input_Port *ip;
          ip = scheme_input_port_record(port);
          scheme_raise_exn(MZEXN_FAIL,
                           "default-load-handler: expected a `module' declaration\n"
                           "  found: %T\n"
                           "  in: %V",
                           err_msg,
                           ip->name);
        }

	return NULL;
      }

      /* Check no more expressions: */
      if (!skip_no_more_check) {
        d = scheme_internal_read(port, lhd->stxsrc, -1, 0, 0, 0, -1, NULL, NULL, NULL, NULL);
        if (!SCHEME_EOFP(d)) {
          Scheme_Input_Port *ip;
          ip = scheme_input_port_record(port);
          scheme_raise_exn(MZEXN_FAIL,
                           "default-load-handler: expected only a `module' declaration;\n"
                           " found an extra form\n"
                           "  in: %V",
                           modname,
                           ip->name);

          return NULL;
        }
      }

      if (!m) {
	/* Replace `module' in read expression with one bound to #%kernel's `module': */
	a = SCHEME_STX_CAR(obj);
	d = SCHEME_STX_CDR(obj);
	a = scheme_datum_to_syntax(module_symbol, a, 
                                   scheme_sys_wraps_phase(scheme_make_integer(genv->phase)), 
                                   0, 1);
	d = scheme_make_pair(a, d);
	obj = scheme_datum_to_syntax(d, obj, scheme_false, 0, 1);
        as_module = 1;
      }
    } else {
      /* Add #%top-interaction, since we're in non-module mode: */
      Scheme_Object *a;
      a = scheme_make_pair(scheme_intern_symbol("#%top-interaction"), obj);
      obj = scheme_datum_to_syntax(a, obj, scheme_false, 0, 0);
    }

    /* ... end special support for module loading ... */

    if (!as_module && genv->stx_context)
      obj = scheme_top_introduce(obj, genv);

    last_val = _scheme_apply_multi_with_prompt(scheme_get_param(config, MZCONFIG_EVAL_HANDLER),
                                               1, &obj);

    /* If multi, we must save then: */
    if (last_val == SCHEME_MULTIPLE_VALUES) {
      save_array = p->ku.multiple.array;
      save_count = p->ku.multiple.count;

      if (SAME_OBJ(save_array, p->values_buffer))
	p->values_buffer = NULL;
    }

    if (SCHEME_SYMBOLP(modname))
      break;
  }

  if (SCHEME_SYMBOLP(modname) && !got_one) {
    Scheme_Input_Port *ip;
    ip = scheme_input_port_record(port);
    scheme_raise_exn(MZEXN_FAIL,
		     "default-load-handler: expected a `module' declaration;\n"
                     " found end-of-file\n"
                     "  in: %V",
		     modname,
		     ip->name);

    return NULL;
  }

  if (save_array) {
    p->ku.multiple.array = save_array;
    p->ku.multiple.count = save_count;
  }

  return last_val;
}

static int nonempty_symbol_list(Scheme_Object *p)
{
  if (!SCHEME_PAIRP(p)) return 0;
  while (SCHEME_PAIRP(p)) {
    if (!SCHEME_SYMBOLP(SCHEME_CAR(p))) return 0;
    p = SCHEME_CDR(p);
  }
  return SCHEME_NULLP(p);
}

static Scheme_Object *default_load(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *name, *expected_module, *v;
  int use_delay_load;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Config *config;
  LoadHandlerData *lhd;
  Scheme_Cont_Frame_Data cframe;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract("default-load-handler", "path-string?", 0, argc, argv);
  expected_module = argv[1];
  if (!SCHEME_FALSEP(expected_module) 
      && !SCHEME_SYMBOLP(expected_module)
      && (!SCHEME_PAIRP(expected_module)
          || (!SCHEME_FALSEP(SCHEME_CAR(expected_module))
              && !SCHEME_SYMBOLP(SCHEME_CAR(expected_module)))
          || !nonempty_symbol_list(SCHEME_CDR(expected_module))))
    scheme_wrong_contract("default-load-handler", 
                          "(or/c #f symbol? (cons/c (or/c #f symbol?) (non-empty-listof symbol?)))",
                          1, argc, argv);

  port = scheme_do_open_input_file("default-load-handler", 0, 1, argv, 0, NULL, NULL, SCHEME_TRUEP(expected_module));

  /* Turn on line/column counting, unless it's a .zo file: */
  if (SCHEME_PATHP(argv[0])) {
    intptr_t len;

    len = SCHEME_BYTE_STRLEN_VAL(argv[0]);
    if ((len < 3)
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 3] != '.')
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 2] != 'z')
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 1] != 'o'))
      scheme_count_lines(port);
  } else {
    intptr_t len;

    len = SCHEME_CHAR_STRLEN_VAL(argv[0]);
    if ((len < 3)
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 3] != '.')
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 2] != 'z')
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 1] != 'o'))
      scheme_count_lines(port);
  }

  config = scheme_current_config();

  v = scheme_get_param(config, MZCONFIG_LOAD_DELAY_ENABLED);
  use_delay_load = SCHEME_TRUEP(v);

  if (SCHEME_TRUEP(expected_module)) {
    config = scheme_extend_config(config, MZCONFIG_CASE_SENS, 
                                  (scheme_case_sensitive ? scheme_true : scheme_false)); /* for legacy code */
    config = scheme_extend_config(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CURLY_BRACES_ARE_PARENS, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_GRAPH, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_COMPILED, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_BOX, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_DOT, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_INFIX_DOT, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_QUASI, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_READER, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_LANG, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_READ_DECIMAL_INEXACT, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_READTABLE, scheme_false);
    config = scheme_extend_config(config, MZCONFIG_READ_CDOT, scheme_false);
    config = scheme_extend_config(config, MZCONFIG_SQUARE_BRACKETS_ARE_TAGGED, scheme_false);
    config = scheme_extend_config(config, MZCONFIG_CURLY_BRACES_ARE_TAGGED, scheme_false);
  } else {
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_COMPILED, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_READER, scheme_true);
    config = scheme_extend_config(config, MZCONFIG_CAN_READ_LANG, scheme_true);
  }

  if (use_delay_load) {
    v = scheme_path_to_complete_path(argv[0], NULL);
    config = scheme_extend_config(config, MZCONFIG_DELAY_LOAD_INFO, v);
  }

  lhd = MALLOC_ONE_RT(LoadHandlerData);
#ifdef MZTAG_REQUIRED
  lhd->type = scheme_rt_load_handler_data;
#endif
  lhd->p = p;
  lhd->config = config;
  lhd->port = port;
  name = scheme_input_port_record(port)->name;
  lhd->stxsrc = name;
  lhd->expected_module = expected_module;

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL, do_load_handler, post_load_handler,
			  NULL, (void *)lhd);

  scheme_pop_continuation_frame(&cframe);

  return v;
}

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[],
				     char *who, int handler_param)
{
  const char *filename;
  Scheme_Object *load_dir, *a[2], *filename_path, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_contract(who, "path-string?", 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   who,
					   NULL,
					   SCHEME_GUARD_FILE_READ);

  /* Calculate load directory */
  load_dir = scheme_get_file_directory(filename);

  filename_path = scheme_make_sized_path((char *)filename, -1, 0);

  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_LOAD_DIRECTORY,
				load_dir);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  a[0] = filename_path;
  a[1] = scheme_false;
  v = _scheme_apply_multi(scheme_get_param(config, handler_param), 2, a);

  scheme_pop_continuation_frame(&cframe);

  return v;
}

static Scheme_Object *load(int argc, Scheme_Object *argv[])
{
  return scheme_load_with_clrd(argc, argv, "load", MZCONFIG_LOAD_HANDLER);
}

static Scheme_Object *
current_load(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load",
			     scheme_make_integer(MZCONFIG_LOAD_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
current_load_use_compiled(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load/use-compiled",
			     scheme_make_integer(MZCONFIG_LOAD_COMPILED_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *abs_directory_p(const char *name, Scheme_Object *d)
{
  if (!SCHEME_FALSEP(d)) {
    char *expanded;
    Scheme_Object *ed;
    char *s;
    int len;

    if (!SCHEME_PATH_STRINGP(d))
      return NULL;

    ed = (SCHEME_PATHP(d) ? d : scheme_char_string_to_path(d));
    s = SCHEME_BYTE_STR_VAL(ed);
    len = SCHEME_BYTE_STRTAG_VAL(ed);

    if (!scheme_is_complete_path(s, len, SCHEME_PLATFORM_PATH_KIND))
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: path is not a complete path\n"
                       "  path: %q",
		       name,
		       s);

    expanded = scheme_expand_string_filename(d, name, NULL,
					     SCHEME_GUARD_FILE_EXISTS);
    ed = scheme_make_sized_path(expanded, strlen(expanded), 1);

    return ed;
  }

  return scheme_false;
}

static Scheme_Object *lr_abs_directory_p(int argc, Scheme_Object **argv)
{
  return abs_directory_p("current-load-relative-directory", argv[0]);
}

static Scheme_Object *
current_load_directory(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-load-relative-directory",
                              scheme_make_integer(MZCONFIG_LOAD_DIRECTORY),
                              argc, argv,
                              -1, lr_abs_directory_p, 
                              "(or/c (and/c path-string? complete-path?) #f)",
                              1);
}

static Scheme_Object *wr_abs_directory_p(int argc, Scheme_Object **argv)
{
  if (SCHEME_PAIRP(argv[0])) {
    Scheme_Object *a, *d, *r;
    a = abs_directory_p("current-write-relative-directory", SCHEME_CAR(argv[0]));
    d = abs_directory_p("current-write-relative-directory", SCHEME_CDR(argv[0]));
    r = scheme_extract_relative_to(a, d, NULL);
    if (SAME_OBJ(a, r)) {
      scheme_contract_error("current-write-relative-directory",
                            "first path does not extend second path",
                            "first path", 1, a,
                            "second path", 1, d,
                            NULL);
    }
    return scheme_make_pair(a, d);
  } else
    return abs_directory_p("current-write-relative-directory", argv[0]);
}

static Scheme_Object *
current_write_directory(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-write-relative-directory",
                              scheme_make_integer(MZCONFIG_WRITE_DIRECTORY),
                              argc, argv,
                              -1, wr_abs_directory_p, 
                              "(or/c (and/c path-string? complete-path?)"
                              /**/ " (cons/c (and/c path-string? complete-path?)"
                              /*        */ " (and/c path-string? complete-path?))"
                              /**/ " #f)",
                              1);
}

#ifdef LOAD_ON_DEMAND
static Scheme_Object *
load_on_demand_enabled(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("load-on-demand-enabled", 
                             scheme_make_integer(MZCONFIG_LOAD_DELAY_ENABLED), 
                             argc, argv, -1, NULL, NULL, 1);
}
#endif

Scheme_Object *scheme_load(const char *file)
{
  Scheme_Object *p[1];
  mz_jmp_buf newbuf, * volatile savebuf;
  Scheme_Object * volatile val;

  p[0] = scheme_make_path(file);
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    val = NULL;
  } else {
    val = scheme_apply_multi(scheme_make_prim((Scheme_Prim *)load),
                             1, p);
  }
  scheme_current_thread->error_buf = savebuf;

  return val;
}

static Scheme_Object *
flush_output(int argc, Scheme_Object *argv[])
{
  Scheme_Object *op;

  if (argc && !SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("flush-output", "output-port?", 0, argc, argv);

  if (argc)
    op = argv[0];
  else
    op = CURRENT_OUTPUT_PORT(scheme_current_config());

  scheme_flush_output(op);

  return (scheme_void);
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_portfun.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_indexed_string, mark_indexed_string);
  GC_REG_TRAV(scheme_rt_load_handler_data, mark_load_handler_data);
  GC_REG_TRAV(scheme_rt_user_input, mark_user_input);
  GC_REG_TRAV(scheme_rt_user_output, mark_user_output);
}

END_XFORM_SKIP;

#endif
