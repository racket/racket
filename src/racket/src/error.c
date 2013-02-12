/*
  Racket
  Copyright (c) 2004-2013 PLT Design Inc.
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
#include <ctype.h>
#ifdef DOS_FILE_SYSTEM
# include <windows.h>
#endif
#ifdef NO_ERRNO_GLOBAL
# define errno -1
#else
# include <errno.h>
#endif
#ifdef USE_C_SYSLOG
# include <syslog.h>
# include <stdarg.h>
#endif

#define mzVA_ARG(x, y) HIDE_FROM_XFORM(va_arg(x, y))
#define TMP_CMARK_VALUE scheme_parameterization_key

#ifndef INIT_SYSLOG_LEVEL
# define INIT_SYSLOG_LEVEL 0
#endif

/* globals */
SHARED_OK scheme_console_printf_t scheme_console_printf;
scheme_console_printf_t scheme_get_console_printf() { return scheme_console_printf; }
void scheme_set_console_printf(scheme_console_printf_t p) { scheme_console_printf = p; }
SHARED_OK Scheme_Exit_Proc scheme_exit;
void scheme_set_exit(Scheme_Exit_Proc p) { scheme_exit = p; }

HOOK_SHARED_OK void (*scheme_console_output)(char *str, intptr_t len);
void scheme_set_console_output(scheme_console_output_t p) { scheme_console_output = p; }

SHARED_OK static Scheme_Object *init_syslog_level = scheme_make_integer(INIT_SYSLOG_LEVEL);
SHARED_OK static Scheme_Object *init_stderr_level = scheme_make_integer(SCHEME_LOG_ERROR);
THREAD_LOCAL_DECL(static Scheme_Logger *scheme_main_logger);
THREAD_LOCAL_DECL(static Scheme_Logger *scheme_gc_logger);
THREAD_LOCAL_DECL(static Scheme_Logger *scheme_future_logger);

/* readonly globals */
READ_ONLY const char *scheme_compile_stx_string = "compile";
READ_ONLY const char *scheme_expand_stx_string = "expand";
READ_ONLY const char *scheme_application_stx_string = "application";
READ_ONLY const char *scheme_set_stx_string = "set!";
READ_ONLY const char *scheme_var_ref_string = "#%variable-reference";
READ_ONLY const char *scheme_begin_stx_string = "begin";
ROSYM static Scheme_Object *none_symbol;
ROSYM static Scheme_Object *fatal_symbol;
ROSYM static Scheme_Object *error_symbol; 
ROSYM static Scheme_Object *warning_symbol;
ROSYM static Scheme_Object *info_symbol;
ROSYM static Scheme_Object *debug_symbol;
ROSYM static Scheme_Object *posix_symbol;
ROSYM static Scheme_Object *windows_symbol;
ROSYM static Scheme_Object *gai_symbol;
ROSYM static Scheme_Object *arity_property;
ROSYM static Scheme_Object *def_err_val_proc;
ROSYM static Scheme_Object *def_error_esc_proc;
ROSYM static Scheme_Object *default_display_handler;
ROSYM static Scheme_Object *emergency_display_handler;
ROSYM static Scheme_Object *def_exe_yield_proc;
READ_ONLY Scheme_Object *scheme_def_exit_proc;
READ_ONLY Scheme_Object *scheme_raise_arity_error_proc;


#ifdef MEMORY_COUNTING_ON
intptr_t scheme_misc_count;
#endif

/* locals */
static Scheme_Object *error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_user_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_syntax_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_type_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_argument_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_result_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_mismatch_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_arguments_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_range_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_arity_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *error_escape_handler(int, Scheme_Object *[]);
static Scheme_Object *error_display_handler(int, Scheme_Object *[]);
static Scheme_Object *error_value_string_handler(int, Scheme_Object *[]);
static Scheme_Object *exit_handler(int, Scheme_Object *[]);
static Scheme_Object *exe_yield_handler(int, Scheme_Object *[]);
static Scheme_Object *error_print_width(int, Scheme_Object *[]);
static Scheme_Object *error_print_context_length(int, Scheme_Object *[]);
static Scheme_Object *error_print_srcloc(int, Scheme_Object *[]);
static Scheme_Object *def_error_escape_proc(int, Scheme_Object *[]);
static Scheme_Object *def_error_display_proc(int, Scheme_Object *[]);
static Scheme_Object *emergency_error_display_proc(int, Scheme_Object *[]);
static Scheme_Object *def_error_value_string_proc(int, Scheme_Object *[]);
static Scheme_Object *def_exit_handler_proc(int, Scheme_Object *[]);
static Scheme_Object *default_yield_handler(int, Scheme_Object *[]);

static Scheme_Object *log_message(int argc, Scheme_Object *argv[]);
static Scheme_Object *log_level_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *log_max_level(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_logger(int argc, Scheme_Object *argv[]);
static Scheme_Object *logger_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_logger(int argc, Scheme_Object *argv[]);
static Scheme_Object *logger_name(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_log_reader(int argc, Scheme_Object *argv[]);
static Scheme_Object *log_reader_p(int argc, Scheme_Object *argv[]);
static int log_reader_get(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);

static Scheme_Object *do_raise(Scheme_Object *arg, int need_debug, int barrier);
static Scheme_Object *nested_exn_handler(void *old_exn, int argc, Scheme_Object *argv[]);

static void update_want_level(Scheme_Logger *logger);

static Scheme_Object *check_arity_property_value_ok(int argc, Scheme_Object *argv[]);

static char *make_provided_list(Scheme_Object *o, int count, intptr_t *lenout);

static char *init_buf(intptr_t *len, intptr_t *blen);
void scheme_set_logging(int syslog_level, int stderr_level)
{
  if (syslog_level > -1)
    init_syslog_level = scheme_make_integer(syslog_level);
  if (stderr_level > -1)
    init_stderr_level = scheme_make_integer(stderr_level);
}

void scheme_set_logging_spec(Scheme_Object *syslog_level, Scheme_Object *stderr_level)
{
  /* A spec is (list* <int> <byte-string> .... <int>) */
  if (syslog_level) {
    REGISTER_SO(init_syslog_level);
    init_syslog_level = syslog_level;
  }
  if (stderr_level) {
    REGISTER_SO(init_stderr_level);
    init_stderr_level = stderr_level;
  }
}

void scheme_init_logging_once(void)
{
  /* Convert specs to use symbols */
  int j;
  Scheme_Object *l, *s;

  for (j = 0; j < 2; j++) {
    l = (j ? init_stderr_level : init_syslog_level);
    if (l) {
      while (!SCHEME_INTP(l)) {
        l = SCHEME_CDR(l);
        s = scheme_intern_exact_symbol(SCHEME_BYTE_STR_VAL(SCHEME_CAR(l)),
                                       SCHEME_BYTE_STRLEN_VAL(SCHEME_CAR(l)));
        SCHEME_CAR(l) = s;
        l = SCHEME_CDR(l);
      }
    }
  }
}

typedef struct {
  int args;
  Scheme_Object *type;
  Scheme_Object **names;
  int count;
  Scheme_Object *exptime;
  int super_pos;
} exn_rec;

#define _MZEXN_TABLE
#include "schexn.h"
#undef _MZEXN_TABLE

static void default_printf(char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  HIDE_FROM_XFORM(va_start(args, msg));
  vfprintf(stderr, msg, args);
  HIDE_FROM_XFORM(va_end(args));
  fflush(stderr);
}

static void default_output(char *s, intptr_t len)
{
  fwrite(s, len, 1, stderr);
  fflush(stderr);
}

intptr_t scheme_errno() {
#ifdef WINDOWS_FILE_HANDLES
  return GetLastError();
#else
  return errno;
#endif
}

Scheme_Config *scheme_init_error_escape_proc(Scheme_Config *config)
{
  if (!def_error_esc_proc) {
    REGISTER_SO(def_error_esc_proc);
    def_error_esc_proc =
      scheme_make_prim_w_arity(def_error_escape_proc,
			       "default-error-escape-handler",
			       0, 0);
  }

  if (config)
    return scheme_extend_config(config, MZCONFIG_ERROR_ESCAPE_HANDLER, def_error_esc_proc);
  else {
    scheme_set_root_param(MZCONFIG_ERROR_ESCAPE_HANDLER, def_error_esc_proc);
    return NULL;
  }
}

/*
  Recognized by scheme_[v]sprintf:

  %c = unicode char
  %d = int
  %gd = long int
  %gx = long int
  %ld = intptr_t
  %lx = intptr_t
  %o = int, octal
  %f = double
  %% = percent

  %s = string
  %5 = mzchar string
  %S = Scheme symbol
  %t = string with inptr_t size
  %u = mzchar string with intptr_t size
  %T = Scheme string
  %q = truncated-to-256 string
  %Q = truncated-to-256 Scheme string
  %V = scheme_value
  %@ = list of scheme_value to write splice
  %D = scheme value to display
  %_ = skip pointer
  %- = skip int

  %L = line number as intptr_t, -1 means no line
  %e = error number for strerror()
  %E = error number for platform-specific error string
  %Z = potential platform-specific error number; additional char*
       is either NULL or a specific error message
  %N = boolean then error number like %E (if boolean is 0)
       or error number for scheme_hostname_error()
  %m = boolean then error number like %e, which
       is used only if the boolean is 1
  %M = boolean then error number like %E, which
       is used only if the boolean is 1
*/

static intptr_t sch_vsprintf(char *s, intptr_t maxlen, const char *msg, va_list args, char **_s,
                             Scheme_Object **_errno_val)
/* NULL for s means allocate the buffer here (and return in (_s), but this function 
   doesn't allocate before extracting arguments from the stack. */
{
  intptr_t i, j;
  char buf[100];

  /* Since we might malloc, move all pointers into a local array for
     the sake of precise GC. We have to do numbers, too, for
     consistency. */

  int pp = 0, ip = 0, dp = 0;
  void *ptrs[25];
  intptr_t ints[25];
  double dbls[25];

  for (j = 0; msg[j]; j++) {
    if (msg[j] == '%') {
      int type;

      j++;
      type = msg[j];

      switch (type) {
      case 'c':
	ints[ip++] = mzVA_ARG(args, int);
	break;
      case 'd':
      case 'o':
      case '-':
	ints[ip++] = mzVA_ARG(args, int);
	break;
      case 'g':
	ints[ip++] = mzVA_ARG(args, long);
	break;
      case 'l':
	ints[ip++] = mzVA_ARG(args, intptr_t);
	break;
      case 'f':
	dbls[dp++] = mzVA_ARG(args, double);
	break;
      case 'L':
	ints[ip++] = mzVA_ARG(args, intptr_t);
	break;
      case 'e':
      case 'E':
	ints[ip++] = mzVA_ARG(args, int);
	break;
      case 'N':
      case 'm':
      case 'M':
	ints[ip++] = mzVA_ARG(args, int);
	ints[ip++] = mzVA_ARG(args, int);
	break;
      case 'Z':
	ints[ip++] = mzVA_ARG(args, int);
	ptrs[pp++] = mzVA_ARG(args, char*);
	break;
      case 'S':
      case 'V':
      case '@':
      case 'D':
      case 'T':
      case 'Q':
      case '_':
	ptrs[pp++] = mzVA_ARG(args, Scheme_Object*);
	break;
      default:
	ptrs[pp++] = mzVA_ARG(args, char*);
	if ((type == 't') || (type == 'u')) {
	  ints[ip++] = mzVA_ARG(args, intptr_t);
	}
      }
    }
  }
  pp = 0;
  ip = 0;
  dp = 0;

  if (!s) {
    s = init_buf(NULL, &maxlen);
    *_s = s;
  }

  --maxlen;

  i = j = 0;
  while ((i < maxlen) && msg[j]) {
    if (msg[j] == '%') {
      int type;

      j++;
      type = msg[j++];

      if (type == '%')
	s[i++] = '%';
      else {
	const char *t;
	intptr_t tlen;
	int dots = 0;

	switch (type) {
	case 'c':
	  {
	    int c;
	    c = ints[ip++];
	    if (c < 128) {
	      buf[0] = c;
	      tlen = 1;
	    } else {
	      mzchar mc;
	      tlen = scheme_utf8_encode_all(&mc, 1, (unsigned char *)buf);
	      c = (int)mc;
	    }
	    t = buf;
	  }
	  break;
	case 'd':
	  {
	    int d;
	    d = ints[ip++];
	    sprintf(buf, "%d", d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case '-':
	  {
	    ip++;
	    t = "";
	    tlen = 0;
	  }
	  break;
	case 'o':
	  {
	    int d;
	    d = ints[ip++];
	    sprintf(buf, "%o", d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'l':
	case 'g':
	  {
	    intptr_t d;
            int as_hex;
            as_hex = (msg[j] == 'x');
	    j++;
	    d = ints[ip++];
            if (as_hex)
              sprintf(buf, "%" PRIxPTR, d);
            else
              sprintf(buf, "%" PRIdPTR, d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'f':
	  {
	    double f;
	    f = dbls[dp++];
	    sprintf(buf, "%f", f);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'L':
	  {
	    intptr_t d;
	    d = ints[ip++];
	    if (d >= 0) {
	      sprintf(buf, "%" PRIdPTR ":", d);
	      t = buf;
	      tlen = strlen(t);
	    } else {
	      t = ":";
	      tlen = 1;
	    }
	  }
	  break;
	case 'e':
        case 'm':
	case 'E':
        case 'M':
	case 'Z':
	case 'N':
	  {
	    int en, he, none = 0;
	    char *es;
            Scheme_Object *err_kind = NULL;
            
	    if (type == 'm') {
              none = !ints[ip++];
	      type = 'e';
              he = 0;
	    } else if (type == 'M') {
              none = !ints[ip++];
	      type = 'E';
              he = 0;
	    } else if (type == 'N') {
	      he = ints[ip++];
	      type = 'E';
	    } else
	      he = 0;

	    en = ints[ip++];

	    if (type == 'Z')
	      es = ptrs[pp++];
	    else
	      es = NULL;

	    if (he) {
	      es = (char *)scheme_hostname_error(en);
              err_kind = gai_symbol;
            }

	    if ((en || es) && !none) {
#ifdef NO_STRERROR_AVAILABLE
	      if (!es)
		es = "Unknown error";
              err_kind = posix_symbol;
#else
# ifdef DOS_FILE_SYSTEM
	      wchar_t mbuf[256];
              int len;
	      if ((type != 'e') && !es) {
		if ((len = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, NULL,
                                          en, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                                          mbuf, 255, NULL))) {
		  int i;
                  i = scheme_utf8_encode((const unsigned int *)mbuf, 0, len, NULL, 0, 1);
                  es = (char *)scheme_malloc_atomic(i + 1);
                  (void)scheme_utf8_encode((const unsigned int *)mbuf, 0, len, es, 0, 1);
                  es[i] = 0;
		  /* Remove newlines: */
		  for (i = strlen(es) - 1; i > 0; i--) {
		    if (isspace(es[i]))
		      es[i] = 0;
		    else
		      break;
		  }
                  err_kind = windows_symbol;
		}
	      }
# endif
	      if (!es) {
		es = strerror(en);
                err_kind = posix_symbol;
              }
#endif
	      tlen = strlen(es) + 24;
	      t = (const char *)scheme_malloc_atomic(tlen);
	      sprintf((char *)t, "%s; errno=%d", es, en);
	      tlen = strlen(t);
              if (_errno_val) {
                err_kind = scheme_make_pair(scheme_make_integer_value(en), err_kind);
                *_errno_val = err_kind;
              }
	    } else {
              if (none) {
                t = "";
                tlen = 0;
              } else {
                t = "errno=?";
                tlen = 7;
              }
	    }

	  }
	  break;
	case 'S':
	  {
	    Scheme_Object *sym;
	    sym = (Scheme_Object *)ptrs[pp++];
	    t = scheme_symbol_name_and_size(sym, (uintptr_t *)&tlen, 0);
	  }
	  break;
	case 'V':
	  {
	    Scheme_Object *o;
	    o = (Scheme_Object *)ptrs[pp++];
	    t = scheme_make_provided_string(o, 1, &tlen);
	  }
	  break;
	case '@':
	  {
	    Scheme_Object *o;
	    o = (Scheme_Object *)ptrs[pp++];
	    t = make_provided_list(o, 1, &tlen);
	  }
	  break;
	case 'D':
	  {
	    Scheme_Object *o;
            intptr_t dlen;
	    o = (Scheme_Object *)ptrs[pp++];
	    t = scheme_display_to_string(o, &dlen);
            tlen = dlen;
	  }
	  break;
        case '_':
          {
            pp++;
            t = "";
            tlen = 0;
          }
          break;
	case 'T':
	case 'Q':
	  {
	    Scheme_Object *str;
	    str = (Scheme_Object *)ptrs[pp++];
	    if (SCHEME_CHAR_STRINGP(str))
	      str = scheme_char_string_to_byte_string(str);
	    t = SCHEME_BYTE_STR_VAL(str);
	    tlen = SCHEME_BYTE_STRLEN_VAL(str);
	  }
	  break;
	case 'u':
	case '5':
	  {
	    mzchar *u;
	    intptr_t ltlen;
	    u = (mzchar *)ptrs[pp++];
	    if (type == 'u') {
	      tlen = ints[ip++];
	      if (tlen < 0)
		tlen = scheme_char_strlen(u);
	    } else {
	      tlen = scheme_char_strlen(u);
	    }
	    t = scheme_utf8_encode_to_buffer_len(u, tlen, NULL, 0, &ltlen);
	    tlen = ltlen;
	  }
	  break;
	default:
	  {
	    t = (char *)ptrs[pp++];
	    if (type == 't') {
	      tlen = ints[ip++];
	      if (tlen < 0)
		tlen = strlen(t);
	    } else {
	      tlen = strlen(t);
	    }
	  }
	  break;
	}

	if ((type == 'q') || (type == 'Q')) {
	  if (tlen > 256) {
	    tlen = 250;
	    dots = 1;
	  }
	}

	while (tlen && i < maxlen) {
	  s[i++] = *t;
	  t = t XFORM_OK_PLUS 1;
	  tlen--;
	}

	if (dots) {
	  /* FIXME: avoiding truncating in the middle of a UTF-8 encoding */
	  if (i < maxlen - 3) {
	    s[i++] = '.';
	    s[i++] = '.';
	    s[i++] = '.';
	  }
	}
      }
    } else {
      s[i++] = msg[j++];
    }
  }

  s[i] = 0;

  return i;
}

static intptr_t scheme_sprintf(char *s, intptr_t maxlen, const char *msg, ...)
{
  intptr_t len;
  GC_CAN_IGNORE va_list args;

  HIDE_FROM_XFORM(va_start(args, msg));
  len = sch_vsprintf(s, maxlen, msg, args, NULL, NULL);
  HIDE_FROM_XFORM(va_end(args));

  return len;
}

void scheme_init_error(Scheme_Env *env)
{
  if (!scheme_console_printf)
    scheme_console_printf = default_printf;
  if (!scheme_console_output)
    scheme_console_output = default_output;

  REGISTER_SO(scheme_raise_arity_error_proc);

  /* errors */
  GLOBAL_NONCM_PRIM("error",                      error,                 1, -1, env);
  GLOBAL_NONCM_PRIM("raise-user-error",           raise_user_error,      1, -1, env);
  GLOBAL_NONCM_PRIM("raise-syntax-error",         raise_syntax_error,    2,  5, env);
  GLOBAL_NONCM_PRIM("raise-type-error",           raise_type_error,      3, -1, env);
  GLOBAL_NONCM_PRIM("raise-argument-error",       raise_argument_error,  3, -1, env);
  GLOBAL_NONCM_PRIM("raise-result-error",         raise_result_error,    3, -1, env);
  GLOBAL_NONCM_PRIM("raise-arguments-error",      raise_arguments_error, 2, -1, env);
  GLOBAL_NONCM_PRIM("raise-mismatch-error",       raise_mismatch_error,  3, -1, env);
  GLOBAL_NONCM_PRIM("raise-range-error",          raise_range_error,     7, 8, env);

  scheme_raise_arity_error_proc =                  scheme_make_noncm_prim(raise_arity_error, "raise-arity-error", 2, -1);
  scheme_add_global_constant("raise-arity-error",  scheme_raise_arity_error_proc, env);

  GLOBAL_PARAMETER("error-display-handler",       error_display_handler,      MZCONFIG_ERROR_DISPLAY_HANDLER,       env);
  GLOBAL_PARAMETER("error-value->string-handler", error_value_string_handler, MZCONFIG_ERROR_PRINT_VALUE_HANDLER,   env);
  GLOBAL_PARAMETER("error-escape-handler",        error_escape_handler,       MZCONFIG_ERROR_ESCAPE_HANDLER,        env);
  GLOBAL_PARAMETER("exit-handler",                exit_handler,               MZCONFIG_EXIT_HANDLER,                env);
  GLOBAL_PARAMETER("executable-yield-handler",    exe_yield_handler,          MZCONFIG_EXE_YIELD_HANDLER,           env);
  GLOBAL_PARAMETER("error-print-width",           error_print_width,          MZCONFIG_ERROR_PRINT_WIDTH,           env);
  GLOBAL_PARAMETER("error-print-context-length",  error_print_context_length, MZCONFIG_ERROR_PRINT_CONTEXT_LENGTH,  env);
  GLOBAL_PARAMETER("error-print-source-location", error_print_srcloc,         MZCONFIG_ERROR_PRINT_SRCLOC,          env);

  /* logging */
  GLOBAL_NONCM_PRIM("exit",              scheme_do_exit,  0, 1, env);
  GLOBAL_NONCM_PRIM("log-level?",        log_level_p,     2, 2, env);
  GLOBAL_NONCM_PRIM("log-max-level",     log_max_level,   1, 1, env);
  GLOBAL_NONCM_PRIM("make-logger",       make_logger,     0, 3, env);
  GLOBAL_NONCM_PRIM("make-log-receiver", make_log_reader, 2, -1, env);

  GLOBAL_PRIM_W_ARITY("log-message",    log_message,   4, 5, env);
  GLOBAL_FOLDING_PRIM("logger?",        logger_p,      1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("logger-name",    logger_name,   1, 1, 1, env);
  GLOBAL_FOLDING_PRIM("log-receiver?",  log_reader_p,  1, 1, 1, env);

  GLOBAL_PARAMETER("current-logger",    current_logger, MZCONFIG_LOGGER, env);

  scheme_add_evt(scheme_log_reader_type, (Scheme_Ready_Fun)log_reader_get, NULL, NULL, 1);

  REGISTER_SO(scheme_def_exit_proc);
  REGISTER_SO(default_display_handler);
  REGISTER_SO(emergency_display_handler);

  scheme_def_exit_proc = scheme_make_prim_w_arity(def_exit_handler_proc, "default-exit-handler", 1, 1);
  default_display_handler = scheme_make_prim_w_arity(def_error_display_proc, "default-error-display-handler", 2, 2);
  emergency_display_handler = scheme_make_prim_w_arity(emergency_error_display_proc, "emergency-error-display-handler", 2, 2);
  

  REGISTER_SO(def_err_val_proc);
  def_err_val_proc = scheme_make_prim_w_arity(def_error_value_string_proc, "default-error-value->string-handler", 2, 2);

  REGISTER_SO(none_symbol);
  REGISTER_SO(fatal_symbol);
  REGISTER_SO(error_symbol);
  REGISTER_SO(warning_symbol);
  REGISTER_SO(info_symbol);
  REGISTER_SO(debug_symbol);
  none_symbol    = scheme_intern_symbol("none");
  fatal_symbol    = scheme_intern_symbol("fatal");
  error_symbol    = scheme_intern_symbol("error");
  warning_symbol  = scheme_intern_symbol("warning");
  info_symbol     = scheme_intern_symbol("info");
  debug_symbol    = scheme_intern_symbol("debug");

  REGISTER_SO(posix_symbol);
  REGISTER_SO(windows_symbol);
  REGISTER_SO(gai_symbol);
  posix_symbol    = scheme_intern_symbol("posix");
  windows_symbol  = scheme_intern_symbol("windows");
  gai_symbol      = scheme_intern_symbol("gai");

  REGISTER_SO(arity_property);
  {
    Scheme_Object *guard;
    guard = scheme_make_prim_w_arity(check_arity_property_value_ok, "guard-for-prop:arity-string", 2, 2);
    arity_property = scheme_make_struct_type_property_w_guard(scheme_intern_symbol("arity-string"), guard);
  }
                                                            
  scheme_add_global_constant("prop:arity-string", arity_property, env);

  REGISTER_SO(def_exe_yield_proc);
  def_exe_yield_proc = scheme_make_prim_w_arity(default_yield_handler,
                                                "default-executable-yield-handler",
                                                1, 1);
}

void scheme_init_logger()
{
  REGISTER_SO(scheme_main_logger);
  scheme_main_logger = scheme_make_logger(NULL, NULL);
  scheme_main_logger->syslog_level = init_syslog_level;
  scheme_main_logger->stderr_level = init_stderr_level;

  REGISTER_SO(scheme_gc_logger);
  scheme_gc_logger = scheme_make_logger(scheme_main_logger, scheme_intern_symbol("GC"));

  REGISTER_SO(scheme_future_logger);
  scheme_future_logger = scheme_make_logger(scheme_main_logger, scheme_intern_symbol("future"));
}

Scheme_Logger *scheme_get_main_logger() {
  return scheme_main_logger;
}

Scheme_Logger *scheme_get_gc_logger() {
  return scheme_gc_logger;
}

Scheme_Logger *scheme_get_future_logger() {
  return scheme_future_logger;
}

void scheme_init_error_config(void)
{
  scheme_set_root_param(MZCONFIG_EXIT_HANDLER, scheme_def_exit_proc);
  scheme_set_root_param(MZCONFIG_ERROR_DISPLAY_HANDLER, default_display_handler);
  scheme_set_root_param(MZCONFIG_ERROR_PRINT_VALUE_HANDLER, def_err_val_proc);
  scheme_set_root_param(MZCONFIG_EXE_YIELD_HANDLER, def_exe_yield_proc);
}

void scheme_init_logger_config() {
  scheme_set_root_param(MZCONFIG_LOGGER, (Scheme_Object *)scheme_main_logger);
}

static void
call_error(char *buffer, int len, Scheme_Object *exn)
{
  if (scheme_current_thread->constant_folding) {
    if (scheme_current_thread->constant_folding != (Optimize_Info *)scheme_false)
      scheme_log(scheme_optimize_info_logger(scheme_current_thread->constant_folding),
                 SCHEME_LOG_WARNING,
                 0,
                 "constant-fold attempt failed%s: %s",
                 scheme_optimize_info_context(scheme_current_thread->constant_folding),
                 buffer);
    if (SCHEME_CHAPERONE_STRUCTP(exn)
        && scheme_is_struct_instance(exn_table[MZEXN_BREAK].type, exn)) {
      /* remember to re-raise exception */
      scheme_current_thread->reading_delayed = exn;
    }
    scheme_longjmp(scheme_error_buf, 1);
  } else if (scheme_current_thread->reading_delayed) {
    scheme_current_thread->reading_delayed = exn;
    scheme_longjmp(scheme_error_buf, 1);
  } else {
    mz_jmp_buf savebuf;
    Scheme_Object *p[2], *display_handler, *escape_handler, *v;
    Scheme_Config *config, *orig_config;
    Scheme_Cont_Frame_Data cframe, cframe2;

    /* For last resort: */
    memcpy((void *)&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

    orig_config = scheme_current_config();
    display_handler = scheme_get_param(orig_config, MZCONFIG_ERROR_DISPLAY_HANDLER);
    escape_handler = scheme_get_param(orig_config, MZCONFIG_ERROR_ESCAPE_HANDLER);
    
    v = scheme_make_byte_string_without_copying("error display handler");
    v = scheme_make_closed_prim_w_arity(nested_exn_handler,
					scheme_make_pair(v, exn),
					"nested-exception-handler", 
					1, 1);

    config = orig_config;
    if (SAME_OBJ(display_handler, default_display_handler))
      config = scheme_extend_config(config,
				    MZCONFIG_ERROR_DISPLAY_HANDLER,
				    emergency_display_handler);
    else
      config = scheme_extend_config(config,
				    MZCONFIG_ERROR_DISPLAY_HANDLER,
				    default_display_handler);
    
    scheme_push_continuation_frame(&cframe);
    scheme_install_config(config);
    scheme_set_cont_mark(scheme_exn_handler_key, v);
    scheme_push_break_enable(&cframe2, 0, 0);

    if (SCHEME_CHAPERONE_STRUCTP(exn)
        && (scheme_is_struct_instance(exn_table[MZEXN_BREAK_HANG_UP].type, exn))) {
      /* skip printout */
    } else {
      p[0] = scheme_make_immutable_sized_utf8_string(buffer, len);
      p[1] = exn;
      scheme_apply_multi(display_handler, 2, p);
    }

    if (SCHEME_CHAPERONE_STRUCTP(exn)
        && (scheme_is_struct_instance(exn_table[MZEXN_BREAK_HANG_UP].type, exn)
            || scheme_is_struct_instance(exn_table[MZEXN_BREAK_TERMINATE].type, exn))) {
      /* Default uncaught exception handler exits on `exn:break:hang-up'
         or `exn:break:terminate'. */
      p[0] = scheme_make_integer(1);
      scheme_do_exit(1, p);
      /* Fall through to regular escape if the exit handler doesn't exit/escape. */
    }

    v = scheme_make_byte_string_without_copying("error escape handler");
    v = scheme_make_closed_prim_w_arity(nested_exn_handler,
					scheme_make_pair(v, exn),
					"nested-exception-handler", 
					1, 1);
    
    config = scheme_extend_config(config,
				  MZCONFIG_ERROR_DISPLAY_HANDLER,
				  default_display_handler);
    config = scheme_extend_config(config,
				  MZCONFIG_ERROR_ESCAPE_HANDLER,
				  def_error_esc_proc);
        
    scheme_pop_break_enable(&cframe2, 0);
    scheme_pop_continuation_frame(&cframe);

    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_exn_handler_key, v);
    scheme_install_config(config);
    scheme_push_break_enable(&cframe2, 0, 0);

    /* Typically jumps out of here */
    scheme_apply_multi(escape_handler, 0, NULL);

    scheme_pop_break_enable(&cframe2, 0);
    scheme_pop_continuation_frame(&cframe);

    /* Didn't escape, so fall back to the default escaper: */
    def_error_escape_proc(0, NULL);
  }
}

intptr_t scheme_get_print_width(void)
{
  intptr_t print_width;
  Scheme_Object *w;

  w = scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_WIDTH);
  if (SCHEME_INTP(w))
    print_width = SCHEME_INT_VAL(w);
  else if (SCHEME_BIGNUMP(w))
    print_width = 0x7FFFFFFF;
  else
    print_width = 10000;

  return print_width;
}

static char *init_buf(intptr_t *len, intptr_t *_size)
{
  uintptr_t local_max_symbol_length;
  intptr_t print_width;
  intptr_t size;
  
  local_max_symbol_length = scheme_get_max_symbol_length();
  print_width             = scheme_get_print_width();

  size = (3 * local_max_symbol_length + 500 + 2 * print_width);

  /* out parameters */
  if (len)
    *len = print_width;
  if (_size)
    *_size = size;

  return (char *)scheme_malloc_atomic(size);
}

void
scheme_signal_error (const char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  char *buffer;
  intptr_t len;

  HIDE_FROM_XFORM(va_start(args, msg));
  len = sch_vsprintf(NULL, 0, msg, args, &buffer, NULL);
  HIDE_FROM_XFORM(va_end(args));

  if (scheme_current_thread->current_local_env) {
    char *s2 = " [during expansion]";
    strcpy(buffer + len, s2);
    len += strlen(s2);
  }

  buffer[len] = 0;

  if (scheme_starting_up) {
    buffer[len++] = '\n';
    buffer[len] = 0;
    scheme_console_output(buffer, len);
    exit(0);
  }

#ifndef SCHEME_NO_EXN
  scheme_raise_exn(MZEXN_FAIL, "%t", buffer, len);
#else
  call_error(buffer, len, scheme_false);
#endif
}

void scheme_warning(char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  char *buffer;
  intptr_t len;

  HIDE_FROM_XFORM(va_start(args, msg));
  len = sch_vsprintf(NULL, 0, msg, args, &buffer, NULL);
  HIDE_FROM_XFORM(va_end(args));

  buffer[len++] = '\n';
  buffer[len] = 0;

  scheme_write_byte_string(buffer, len,
			   scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PORT));
}

void scheme_log(Scheme_Logger *logger, int level, int flags,
                const char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  char *buffer;
  intptr_t len;

  if (logger) {
    if (logger->local_timestamp == *logger->timestamp)
      if (logger->want_level < level)
        return;
  }

  HIDE_FROM_XFORM(va_start(args, msg));
  len = sch_vsprintf(NULL, 0, msg, args, &buffer, NULL);
  HIDE_FROM_XFORM(va_end(args));

  buffer[len] = 0;

  scheme_log_message(logger, level, buffer, len, NULL);
}

void scheme_log_w_data(Scheme_Logger *logger, int level, int flags,
                       Scheme_Object *data,
                       const char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  char *buffer;
  intptr_t len;

  if (logger) {
    if (logger->local_timestamp == *logger->timestamp)
      if (logger->want_level < level)
        return;
  }

  HIDE_FROM_XFORM(va_start(args, msg));
  len = sch_vsprintf(NULL, 0, msg, args, &buffer, NULL);
  HIDE_FROM_XFORM(va_end(args));

  buffer[len] = 0;

  scheme_log_message(logger, level, buffer, len, data);
}

int scheme_log_level_p(Scheme_Logger *logger, int level)
{
  if (!logger) {
    Scheme_Config *config;
    config = scheme_current_config();
    logger = (Scheme_Logger *)scheme_get_param(config, MZCONFIG_LOGGER);
  }

  if (logger->local_timestamp < *logger->timestamp)
    update_want_level(logger);

  return (logger->want_level >= level);
}

static char *error_write_to_string_w_max(Scheme_Object *v, int len, intptr_t *lenout)
{
  Scheme_Object *o, *args[2];

  o = scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_VALUE_HANDLER);

  if ((SAME_OBJ(o, def_err_val_proc)
       && SAME_OBJ(scheme_get_param(scheme_current_config(), MZCONFIG_PORT_PRINT_HANDLER),
		   scheme_default_global_print_handler))) {
    intptr_t l;
    char *s;
    s = scheme_print_to_string_w_max(v, &l, len);
    if (lenout)
      *lenout = l;
    return s;
  } else {
    Scheme_Config *config;
    Scheme_Cont_Frame_Data cframe, cframe2;

    args[0] = v;
    args[1] = scheme_make_integer(len);

    config = scheme_extend_config(scheme_current_config(),
				  MZCONFIG_ERROR_PRINT_VALUE_HANDLER,
				  def_err_val_proc);
    config = scheme_extend_config(config,
				  MZCONFIG_PRINT_UNREADABLE,
				  scheme_true);

    scheme_push_continuation_frame(&cframe);
    scheme_install_config(config);
    scheme_push_break_enable(&cframe2, 0, 0);

    o = _scheme_apply(o, 2, args);

    scheme_pop_break_enable(&cframe2, 0);
    scheme_pop_continuation_frame(&cframe);

    if (SCHEME_CHAR_STRINGP(o)) {
      o = scheme_char_string_to_byte_string(o);
    }

    if (SCHEME_BYTE_STRINGP(o)) {
      char *s = SCHEME_BYTE_STR_VAL(o);
      if (SCHEME_BYTE_STRTAG_VAL(o) > len) {
	char *naya;
	naya = scheme_malloc_atomic(len + 1);
	memcpy(naya, s, len);
	s[len] = 0;
	if (lenout)
	  *lenout = len;
      } else if (lenout)
	*lenout = SCHEME_BYTE_STRTAG_VAL(o);
      return s;
    } else {
      if (lenout)
	*lenout = 3;
      return "...";
    }
  }
}

static Scheme_Object *check_arity_property_value_ok(int argc, Scheme_Object *argv[])
{
  if (!scheme_check_proc_arity(NULL, 1, 0, 1, argv))
    scheme_arg_mismatch("guard-for-prop:arity-string",
                        "property value is not a procedure (arity 1): ",
                        argv[0]);
  return argv[0];
}

static char *make_arity_expect_string(const char *name, int namelen,
				      int minc, int maxc,
				      int argc, Scheme_Object **argv,
				      intptr_t *_len, int is_method,
                                      const char *map_name)
/* minc == -1 => name is really a case-lambda, native closure, or proc-struct.
   minc == -2 => use generic arity-mismatch message */
{
  intptr_t len, pos, slen;
  int xargc, xminc, xmaxc;
  char *s, *arity_str = NULL;
  const char *prefix_msg1, *prefix_msg2, *suffix_msg;
  int arity_len = 0;

  s = init_buf(&len, &slen);

  if (!name)
    name = "#<procedure>";

  xargc = argc - (is_method ? 1 : 0);
  xminc = minc - (is_method ? 1 : 0);
  xmaxc = maxc - (is_method ? 1 : 0);

  if ((minc == -1) && SCHEME_CHAPERONE_PROC_STRUCTP((Scheme_Object *)name)) {
    Scheme_Object *arity_maker;

    while (1) {
      arity_maker = scheme_struct_type_property_ref(arity_property, (Scheme_Object *)name);
      if (arity_maker) {
        Scheme_Object *v, *a[1];
        a[0] = (Scheme_Object *)name;
        v = scheme_apply(arity_maker, 1, a);
        if (SCHEME_CHAR_STRINGP(v)) {
          v = scheme_char_string_to_byte_string(v);
          arity_str = SCHEME_BYTE_STR_VAL(v);
          arity_len = SCHEME_BYTE_STRLEN_VAL(v);
          if (arity_len > len)
            arity_len = len;
          name = scheme_get_proc_name((Scheme_Object *)name, &namelen, 1);
          if (!name) {
            name = "#<procedure>";
            namelen = strlen(name);
          }
          break;
        } else
          break;
      } else {
        Scheme_Object *v;
        int is_method;
        v = (Scheme_Object *)name;
        if (SCHEME_CHAPERONEP(v))
          v = SCHEME_CHAPERONE_VAL(v);
        v = scheme_extract_struct_procedure(v, -1, NULL, &is_method);
        if (!v || is_method || !SCHEME_CHAPERONE_PROC_STRUCTP(v))
          break;
        name = (const char *)v;
      }
      SCHEME_USE_FUEL(1);
    }

    if (!arity_str) {
      /* If the arity is something simple, we'll make a good error
         message. Otherwise, we'll just use the "no matching case"
         version. */
      Scheme_Object *arity;
      arity = scheme_arity((Scheme_Object *)name);
      if (SCHEME_INTP(arity)) {
        minc = maxc = SCHEME_INT_VAL(arity);
        xmaxc = xminc = minc - (is_method ? 1 : 0);
        name = scheme_get_proc_name((Scheme_Object *)name, &namelen, 1);
        if (!name) {
          name = "#<procedure>";
          namelen = strlen(name);
        }
      }
    }
  }

  if (map_name) {
    prefix_msg1 = map_name;
    prefix_msg2 = (": argument mismatch;\n"
                   " the given procedure's expected number of arguments does not match\n"
                   " the given number of lists\n"
                   "  given procedure: ");
    suffix_msg = "";
  } else {
    prefix_msg1 = "";
    prefix_msg2 = "";
    suffix_msg = (": arity mismatch;\n"
                  " the expected number of arguments does not match the given number");
  }

  if (arity_str) {
    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  expected: %t\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 name, (intptr_t)namelen, 
                         suffix_msg,
                         arity_str, (intptr_t)arity_len, xargc);
  } else if (minc < 0) {
    const char *n;
    int nlen;

    if (minc == -2) {
      n = name;
      nlen = (namelen < 0 ? strlen(n) : namelen);
    } else
      n = scheme_get_proc_name((Scheme_Object *)name, &nlen, 1);

    if (!n) {
      n = "#<case-lambda-procedure>";
      nlen = strlen(n);
    }

    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 n, (intptr_t)nlen,
                         suffix_msg,
			 xargc);
  } else if (!maxc)
    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  expected: 0\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 name, (intptr_t)namelen, 
                         suffix_msg,
                         xargc);
  else if (maxc < 0)
    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  expected: at least %d\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 name, (intptr_t)namelen, 
                         suffix_msg,
                         xminc, xargc);
  else if (minc == maxc)
    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  expected: %d\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 name, (intptr_t)namelen, 
                         suffix_msg,
                         xminc, xargc);
  else
    pos = scheme_sprintf(s, slen, 
                         "%s%s%t%s\n"
                         "  expected: %d to %d\n"
                         "  given: %d",
                         prefix_msg1, prefix_msg2,
			 name, (intptr_t)namelen, 
                         suffix_msg,
                         xminc, xmaxc, xargc);

  if (xargc && argv) {
    len -= (xargc * 4);
    len /= xargc;
    if ((xargc < 50) && (len >= 3)) {
      int i;

      for (i = (is_method ? 1 : 0); i < argc; i++) {
	intptr_t l;
	char *o;
        if (i == (is_method ? 1 : 0)) {
          strcpy(s + pos, "\n  arguments...:\n   ");
          pos += 20;
        } else {
          strcpy(s + pos, "\n   ");
          pos += 4;
        }

	o = error_write_to_string_w_max(argv[i], len, &l);
	memcpy(s + pos, o, l);
	pos += l;
      }

      s[pos] = 0;
    }
  }

  *_len = pos;

  return s;
}

void scheme_wrong_count_m(const char *name, int minc, int maxc,
			  int argc, Scheme_Object **argv, int is_method)
/* minc == -1 => name is really a proc.
   minc == -2 => use generic "no matching clause" message */
{
  char *s;
  intptr_t len;
  Scheme_Thread *p = scheme_current_thread;

  if (argv == p->tail_buffer) {
    /* See calls in scheme_do_eval: */
    GC_CAN_IGNORE Scheme_Object **tb;
    p->tail_buffer = NULL; /* so args aren't zeroed */
    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
    p->tail_buffer = tb;
  }

  /* minc = 1 -> name is really a case-lambda or native proc */

  if (minc == -1) {
    /* Extract arity, check for is_method in case-lambda, etc. */
    if (SAME_TYPE(SCHEME_TYPE((Scheme_Object *)name), scheme_closure_type)) {
      Scheme_Closure_Data *data;
      data = SCHEME_COMPILED_CLOS_CODE((Scheme_Object *)name);
      name = scheme_get_proc_name((Scheme_Object *)name, NULL, 1);
      
      minc = data->num_params;
      if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
        minc -= 1;
        maxc = -1;
      } else
        maxc = minc;
    } else if (SAME_TYPE(SCHEME_TYPE((Scheme_Object *)name), scheme_case_closure_type)) {
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)name;
      if (cl->count) {
	Scheme_Closure_Data *data;
	data = (Scheme_Closure_Data *)SCHEME_COMPILED_CLOS_CODE(cl->array[0]);
	if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_IS_METHOD)
	  is_method = 1;
      } else if (cl->name && SCHEME_BOXP(cl->name)) {
	/* See note in schpriv.h about the IS_METHOD hack */
	is_method = 1;
      }
#ifdef MZ_USE_JIT
    } else if (SAME_TYPE(SCHEME_TYPE((Scheme_Object *)name), scheme_native_closure_type)) {
      Scheme_Object *pa;
      pa = scheme_get_native_arity((Scheme_Object *)name, -1);
      if (SCHEME_BOXP(pa)) {
	pa = SCHEME_BOX_VAL(pa);
	is_method = 1;
      }
      if (SCHEME_INTP(pa)) {
	minc = SCHEME_INT_VAL(pa);
	if (minc < 0) {
	  minc = (-minc) - 1;
	  maxc = -1;
	} else
	  maxc = minc;
	name = scheme_get_proc_name((Scheme_Object *)name, NULL, 1);
      } else if (SCHEME_STRUCTP(pa)) {
	/* This happens when a non-case-lambda is not yet JITted.
	   It's an arity-at-least record. */
	pa = ((Scheme_Structure *)pa)->slots[0];
	minc = SCHEME_INT_VAL(pa);
	maxc = -1;
	name = scheme_get_proc_name((Scheme_Object *)name, NULL, 1);
      } else {
	/* complex; use "no matching case" msg */
      }
#endif
    }
  }

  /* Watch out for impossible is_method claims: */
  if (!argc || !minc)
    is_method = 0;

  if (maxc > SCHEME_MAX_ARGS)
    maxc = -1;

  s = make_arity_expect_string(name, -1, minc, maxc, argc, argv, &len, is_method, NULL);

  scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY, "%t", s, len);
}

void scheme_wrong_count(const char *name, int minc, int maxc, int argc,
			Scheme_Object **argv)
{
  /* don't allocate here, in case rands == p->tail_buffer */
  scheme_wrong_count_m(name, minc, maxc, argc, argv, 0);
}

void scheme_case_lambda_wrong_count(const char *name,
				    int argc, Scheme_Object **argv,
				    int is_method,
				    int count, ...)
{
  char *s;
  intptr_t len;

  /* Watch out for impossible is_method claims: */
  if (!argc)
    is_method = 0;

  s = make_arity_expect_string(name, -1, -2, 0, argc, argv, &len, is_method, NULL);

  scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY, "%t", s, len);
}

char *scheme_make_arity_expect_string(const char *map_name,
                                      Scheme_Object *proc,
				      int argc, Scheme_Object **argv,
				      intptr_t *_slen)
{
  const char *name;
  int namelen = -1;
  int mina, maxa;

  if (SCHEME_CHAPERONEP(proc)) {
    proc = SCHEME_CHAPERONE_VAL(proc);
  }

  if (SCHEME_PRIMP(proc)) {
    name = ((Scheme_Primitive_Proc *)proc)->name;
    mina = ((Scheme_Primitive_Proc *)proc)->mina;
    if (mina < 0) {
      /* set min1 to -2 to indicates cases */
      mina = -2;
      maxa = 0;
    } else {
      maxa = ((Scheme_Primitive_Proc *)proc)->mu.maxa;
      if (maxa > SCHEME_MAX_ARGS)
	maxa = -1;
    }
  } else if (SCHEME_CLSD_PRIMP(proc)) {
    name = ((Scheme_Closed_Primitive_Proc *)proc)->name;
    mina = ((Scheme_Closed_Primitive_Proc *)proc)->mina;
    maxa = ((Scheme_Closed_Primitive_Proc *)proc)->maxa;
  } else if (SAME_TYPE(SCHEME_TYPE(proc), scheme_case_closure_type)) {
    name = scheme_get_proc_name(proc, &namelen, 1);
    mina = -2;
    maxa = 0;
#ifdef MZ_USE_JIT
  } else if (SAME_TYPE(SCHEME_TYPE((Scheme_Object *)proc), scheme_native_closure_type)) {
    Scheme_Object *pa;
    pa = scheme_get_native_arity((Scheme_Object *)proc, -1);
    if (SCHEME_BOXP(pa)) {
      pa = SCHEME_BOX_VAL(pa);
    }
    if (SCHEME_INTP(pa)) {
      mina = SCHEME_INT_VAL(pa);
      if (mina < 0) {
	mina = (-mina) - 1;
	maxa = -1;
      } else
	maxa = mina;
    } else if (SCHEME_STRUCTP(pa)) {
      /* This happens when a non-case-lambda is not yet JITted.
	 It's an arity-at-least record. */
      pa = ((Scheme_Structure *)pa)->slots[0];
      mina = SCHEME_INT_VAL(pa);
      maxa = -1;
    } else {
      /* complex; use "no matching case" msg */
      mina = -2;
      maxa = 0;
    }
    name = scheme_get_proc_name((Scheme_Object *)proc, &namelen, 1);
#endif
  } else if (SCHEME_CHAPERONE_STRUCTP(proc)) {
    name = (const char *)proc;
    mina = -1;
    maxa = 0;
  } else {
    Scheme_Closure_Data *data;

    data = (Scheme_Closure_Data *)SCHEME_COMPILED_CLOS_CODE(proc);
    mina = maxa = data->num_params;
    if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
      --mina;
      maxa = -1;
    }
    name = scheme_get_proc_name(proc, &namelen, 1);
  }

  return make_arity_expect_string(name, namelen, mina, maxa, argc, argv, _slen, 0, map_name);
}

char *scheme_make_args_string(const char *s, int which, int argc, Scheme_Object **argv, intptr_t *_olen)
{
  char *other;
  intptr_t len;
  GC_CAN_IGNORE char *isres = "arguments";

  other = init_buf(&len, NULL);

  if (argc < 0) {
    isres = "results";
    argc = -argc;
  }

  len /= (argc - (((which >= 0) && (argc > 1)) ? 1 : 0));
  if ((argc < 50) && (len >= 3)) {
    int i, pos;

    sprintf(other, "; %s%s were:", s, isres);
    pos = strlen(other);
    for (i = 0; i < argc; i++) {
      if (i != which) {
	intptr_t l;
	char *o;
	o = error_write_to_string_w_max(argv[i], len, &l);
	memcpy(other + pos, " ", 1);
	memcpy(other + pos + 1, o, l);
	pos += l + 1;
      }
    }
    other[pos] = 0;
    if (_olen)
      *_olen = pos;
  } else {
    sprintf(other, "; given %d arguments total", argc);
    if (_olen)
      *_olen = strlen(other);
  }

  return other;
}

char *scheme_make_arg_lines_string(const char *indent, int which, int argc, Scheme_Object **argv, intptr_t *_olen)
{
  char *other;
  intptr_t len, plen;

  if (!argc || ((argc == 1) && (which == 0))) {
    other = " [none]";
    if (_olen)
      *_olen = strlen(other);    
    return other;
  }

  other = init_buf(&len, NULL);

  plen = strlen(indent);
  
  len -= ((argc - 1) * (plen + 1));
  len /= (argc - (((which >= 0) && (argc > 1)) ? 1 : 0));

  if (len >= 3) {
    int i, pos;

    pos = 0;
    for (i = 0; i < argc; i++) {
      if (i != which) {
	intptr_t l;
	char *o;
      
        memcpy(other + pos, "\n", 1);
        pos++;
        memcpy(other + pos, indent, plen);
        pos += plen;
      
	o = error_write_to_string_w_max(argv[i], len, &l);
	memcpy(other + pos, o, l);
	pos += l;
      }
    }
    other[pos] = 0;
    if (_olen)
      *_olen = pos;
  } else {
    sprintf(other, "... [%d total] ...", argc);
    if (_olen)
      *_olen = strlen(other);
  }

  return other;
}

const char *scheme_number_suffix(int which)
{
  READ_ONLY static char *ending[] = {"st", "nd", "rd"};

  if (!which)
    return "th";
  --which;

  which = which % 100;

  return ((which < 10 || which >= 20)
	  && ((which % 10) < 3)) ? ending[which % 10] : "th";
}

void scheme_wrong_type(const char *name, const char *expected,
		       int which, int argc,
		       Scheme_Object **argv)
{
  Scheme_Object *o;
  char *s;
  intptr_t slen;
  int isres = 0;
  GC_CAN_IGNORE char *isress = "argument";
  GC_CAN_IGNORE char *isgiven = "given";

  o = argv[which < 0 ? 0 : which];
  if (argc < 0) {
    argc = -argc;
    isress = "result";
    isgiven = "received";
    isres = 1;
  }
  if (which == -2) {
    isress = "value";
    isgiven = "received";
  }

  s = scheme_make_provided_string(o, 1, &slen);

  if ((which < 0) || (argc == 1))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: expect%s %s of type <%s>; "
		     "%s: %t",
		     name, 
		     (which < 0) ? "ed" : "s",
		     isress, expected, isgiven,
                     s, slen);
  else {
    char *other;
    intptr_t olen;

    if ((which >= 0) && (argc > 1))
      other = scheme_make_args_string("other ", which,
				      (isres ? -argc : argc),
				      argv, &olen);
    else {
      other = "";
      olen = 0;
    }

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: expects type <%s> as %d%s %s, "
		     "given: %t%t",
		     name, expected, which + 1,
		     scheme_number_suffix(which + 1),
		     isress,
		     s, slen, other, olen);
  }
}

static const char *indent_lines(const char *s, intptr_t *_len, int initial_indent, int amt)
{
  intptr_t len, i, j, lines = 1;
  int a;
  char *s2;

  if (_len)
    len = *_len;
  else
    len = strlen(s);

  for (i = 0; i < len; i++) {
    if (s[i] == '\n')
      lines++;
  }

  if ((len > 72) || (lines > 1)) {
    s2 = scheme_malloc_atomic(len + (lines * (amt + 1)) + 1);

    if (initial_indent) {
      s2[0] = '\n';
      j = 1;
      for (a = 0; a < amt; a++) {
        s2[j++] = ' ';
      }
    } else
      j = 0;

    for (i = 0; i < len; i++) {
      s2[j++] = s[i];
      if (s[i] == '\n') {
        for (a = 0; a < amt; a++) {
          s2[j++] = ' ';
        }
      }
    }
    s2[j] = 0;

    if (_len)
      *_len = j;

    return s2;
  }

  return s;
}

void scheme_wrong_contract(const char *name, const char *expected,
                           int which, int argc,
                           Scheme_Object **argv)
{
  Scheme_Object *o;
  char *s;
  intptr_t slen;
  int isres = 0;
  GC_CAN_IGNORE char *isgiven = "given", *kind = "argument";

  o = argv[which < 0 ? 0 : which];
  if (argc < 0) {
    argc = -argc;
    isgiven = "received";
    kind = "result";
    isres = 1;
  }
  if (which == -2) {
    isgiven = "received";
    kind = "result";
  }
  if (argc == 0)
    kind = "value";

  s = scheme_make_provided_string(o, 1, &slen);

  if ((which < 0) || (argc <= 1))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: contract violation\n"
                     "  expected: %s\n"
                     "  %s: %t",
		     name,
		     indent_lines(expected, NULL, 1, 3),
                     isgiven, s, slen);
  else {
    char *other;
    intptr_t olen;

    other = scheme_make_arg_lines_string("   ", which, argc, argv, &olen);

    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "%s: contract violation\n"
                     "  expected: %s\n"
                     "  %s: %t\n"
                     "  %s position: %d%s\n"
                     "  other %s...:%s",
		     name, 
                     indent_lines(expected, NULL, 1, 3),
		     isgiven, s, slen, 
                     kind, which + 1, scheme_number_suffix(which + 1),
                     (!isres ? "arguments" : "results"), other, olen);
  }
}

void scheme_wrong_field_type(Scheme_Object *c_name,
			     const char *expected,
			     Scheme_Object *o)
{
  const char *s;
  Scheme_Object *a[1];
  a[0] = o;
  s = scheme_symbol_name(c_name);
  scheme_wrong_type(s, expected, -1, 0, a);
}

void scheme_wrong_field_contract(Scheme_Object *c_name,
                                 const char *expected,
                                 Scheme_Object *o)
{
  const char *s;
  Scheme_Object *a[1];
  a[0] = o;
  s = scheme_symbol_name(c_name);
  scheme_wrong_contract(s, expected, -1, 0, a);
}

void scheme_arg_mismatch(const char *name, const char *msg, Scheme_Object *o)
{
  char *s;
  intptr_t slen;
  
  if (o)
    s = scheme_make_provided_string(o, 1, &slen);
  else {
    s = "";
    slen = 0;
  }

  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		   "%s: %s%t",
		   name, msg, s, slen);
}

static void do_out_of_range(const char *name, const char *type, const char *which,
                            int ending,
                            Scheme_Object *i, Scheme_Object *s,
                            Scheme_Object *low_bound, Scheme_Object *sstart, Scheme_Object *slen)
{
  if (!type) {
    type = (SCHEME_BYTE_STRINGP(s) ? "byte string" : "string");
  }
  
  if (!scheme_bin_lt(slen, sstart)) {
    char *sstr;
    intptr_t strlen;
    int small_end = 0;

    if (ending) {
      if (scheme_bin_gt_eq(i, low_bound)
          && scheme_bin_lt(i, sstart))
        small_end = 1;
    }

    sstr = scheme_make_provided_string(s, 2, &strlen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: %sindex is %s\n  %sindex: %s\n  %s%V%s%V]\n  %s: %t",
		     name, which, 
                     small_end ? "smaller than starting index" : "out of range",
		     which, scheme_make_provided_string(i, 2, NULL),
                     ending ? "starting index: " : "valid range: [",
		     sstart, 
                     ending ? "\n  valid range: [0, " : ", ",
                     slen,
		     type,
		     sstr, strlen);
  } else {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: %sindex is out of range for empty %s\n  %sindex: %s",
		     name, which,
		     type, 
                     which, scheme_make_provided_string(i, 0, NULL));
  }
}

void scheme_out_of_range(const char *name, const char *type, const char *which,
                         Scheme_Object *i, Scheme_Object *s,
                         intptr_t start, intptr_t len)
{
  if (start < 0) {
    start = 0;
    len = len - 1;
  }

  do_out_of_range(name, type, which, !strcmp(which, "ending "),
                  i, s, scheme_make_integer(0), scheme_make_integer(start), scheme_make_integer(len));
}

static Scheme_Object *raise_range_error(int argc, Scheme_Object *argv[])
{
  Scheme_Object *type, *desc;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("raise-range-error", "symbol?", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract("raise-range-error", "string?", 1, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[2]))
    scheme_wrong_contract("raise-range-error", "string?", 2, argc, argv);
  if (!SCHEME_INTP(argv[3]) && !SCHEME_BIGNUMP(argv[3]))
    scheme_wrong_contract("raise-range-error", "exact-integer?", 3, argc, argv);
  if (!SCHEME_INTP(argv[5]) && !SCHEME_BIGNUMP(argv[5]))
    scheme_wrong_contract("raise-range-error", "exact-integer?", 5, argc, argv);
  if (!SCHEME_INTP(argv[6]) && !SCHEME_BIGNUMP(argv[6]))
    scheme_wrong_contract("raise-range-error", "exact-integer?", 6, argc, argv);
  if (argc > 7) {
    if (!SCHEME_FALSEP(argv[7]) && !SCHEME_INTP(argv[7]) && !SCHEME_BIGNUMP(argv[7]))
      scheme_wrong_contract("raise-range-error", "(or/c exact-integer? #f)", 7, argc, argv);
  }
  
  type = scheme_char_string_to_byte_string(argv[1]);
  desc = scheme_char_string_to_byte_string(argv[2]);

  do_out_of_range(scheme_symbol_val(argv[0]), 
                  SCHEME_BYTE_STR_VAL(type), /* type */
                  SCHEME_BYTE_STR_VAL(desc), /* index description */
                  ((argc > 7) && SCHEME_TRUEP(argv[7])),
                  argv[3], /* index */
                  argv[4], /* in value */
                  argv[7], /* lower bound */
                  argv[5], /* start */
                  argv[6]); /* end */

  return scheme_void;
}

#define MAX_MISMATCH_EXTRAS 5

void scheme_contract_error(const char *name, const char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  int i, cnt = 0, kind;
  intptr_t len = 0, nlen, mlen, seplen;
  const char *strs[MAX_MISMATCH_EXTRAS], *str, *sep;
  Scheme_Object *vs[MAX_MISMATCH_EXTRAS], *v;
  const char *v_strs[MAX_MISMATCH_EXTRAS], *v_str;
  intptr_t v_str_lens[MAX_MISMATCH_EXTRAS], v_str_len;
  char *s;

  HIDE_FROM_XFORM(va_start(args, msg));
  while (1) {
    str = mzVA_ARG(args, const char *);
    if (!str) break;
    strs[cnt] = str;
    kind = mzVA_ARG(args, int);
    if (kind) {
      v = mzVA_ARG(args, Scheme_Object *);
      vs[cnt++] = v;
    } else {
      str = mzVA_ARG(args, const char *);
      v_strs[cnt] = str;
      v_str_lens[cnt] = strlen(str);
      vs[cnt++] = NULL;
    }
  }
  HIDE_FROM_XFORM(va_end(args));

  for (i = 0; i < cnt; i++) {
    if (vs[i]) {
      v_str = scheme_make_provided_string(vs[i], 1, &v_str_len);
      v_strs[i] = v_str;
      v_str_lens[i] = v_str_len;
    } else
      v_str_len = v_str_lens[i];
    len += v_str_len + 5 + strlen(strs[i]);
  }

  sep = ": ";

  mlen = strlen(msg);
  nlen = strlen(name);
  seplen = strlen(sep);

  len += mlen + nlen + seplen + 10;

  s = scheme_malloc_atomic(len);
  len = 0;
  memcpy(s, name, nlen);
  len += nlen;
  memcpy(s + len, sep, seplen);
  len += seplen;
  memcpy(s + len, msg, mlen);
  len += mlen;
  for (i = 0; i < cnt; i++) {
    memcpy(s + len, "\n  ", 3);
    len += 3;
    nlen = strlen(strs[i]);
    memcpy(s + len, strs[i], nlen);
    len += nlen;
    memcpy(s + len, ": ", 2);
    len += 2;
    memcpy(s + len, v_strs[i], v_str_lens[i]);
    len += v_str_lens[i];
  }
  s[len] = 0;

  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		   "%t",
                   s, len);
}

void scheme_wrong_chaperoned(const char *who, const char *what, Scheme_Object *orig, Scheme_Object *naya)
{
  char buf[128];

  sprintf(buf, 
          "non-chaperone result;\n"
          " received a %s that is not a chaperone of the original %s", 
          what, what);

  scheme_contract_error(who,
                        buf,
                        "original", 1, orig,
                        "received", 1, naya,
                        NULL);
}

void scheme_system_error(const char *name, const char *what, int errid)
{
  scheme_raise_exn(MZEXN_FAIL, 
                   "%s: %s failed\n"
                   "  system error: %e", 
                   name, what, errid);
}

#define MZERR_MAX_SRC_LEN 100

static char *make_srcloc_string(Scheme_Stx_Srcloc *srcloc, intptr_t *len)
{
  intptr_t line, col;
  Scheme_Object *src;
  char *srcstr, *result;
  intptr_t srclen, rlen;

  if (!srcloc->src || (SCHEME_FALSEP(srcloc->src) && (srcloc->pos < 0))) {
    if (len) *len = 0;
    return NULL;
  }

  line = srcloc->line;
  col = srcloc->col;
  if (col < 0)
    col = srcloc->pos;

  src = srcloc->src;

  if (src && SCHEME_PATHP(src)) {
    /* Strip off prefix matching the current directory: */
    src = scheme_remove_current_directory_prefix(src);

    /* Truncate from the front, to get the interesting part of paths: */
    srclen = SCHEME_BYTE_STRLEN_VAL(src);
    if (srclen > MZERR_MAX_SRC_LEN) {
      srcstr = scheme_malloc_atomic(MZERR_MAX_SRC_LEN);
      memcpy(srcstr, SCHEME_BYTE_STR_VAL(src) + (srclen - MZERR_MAX_SRC_LEN),
	     MZERR_MAX_SRC_LEN);
      srcstr[0] = '.';
      srcstr[1] = '.';
      srcstr[2] = '.';
      srclen = MZERR_MAX_SRC_LEN;
    } else
      srcstr = SCHEME_BYTE_STR_VAL(src);
  } else
    srcstr = scheme_display_to_string_w_max(src, &srclen, MZERR_MAX_SRC_LEN);

  result = (char *)scheme_malloc_atomic(srclen + 15);

  if (col >= 0) {
    rlen = scheme_sprintf(result, srclen + 15, "%t:%L%ld",
			  srcstr, srclen, line, col-1);
  } else {
    rlen = scheme_sprintf(result, srclen + 15, "%t::",
			  srcstr, srclen);
  }

  if (len) *len = rlen;
  return result;
}

void scheme_read_err(Scheme_Object *port,
		     Scheme_Object *stxsrc,
		     intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
		     int gotc, Scheme_Object *indentation,
		     const char *detail, ...)
{
  GC_CAN_IGNORE va_list args;
  char *s, *ls, lbuf[30], *fn, *suggests;
  intptr_t slen, fnlen;
  int show_loc;
  Scheme_Object *loc;

  HIDE_FROM_XFORM(va_start(args, detail));
  slen = sch_vsprintf(NULL, 0, detail, args, &s, NULL);
  HIDE_FROM_XFORM(va_end(args));

  ls = "";
  fnlen = 0;

  show_loc = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC));

  /* Via read/recursive, it's possible that the reader will try to
     complain about a character that precedes the start of a port.
     In that case, pos can be 0. */
  if (!pos) line = col = pos = -1;

  if (stxsrc) {
    Scheme_Object *xsrc;

    xsrc = scheme_make_stx_w_offset(scheme_false, line, col, pos, span, stxsrc, STX_SRCTAG);

    stxsrc = ((Scheme_Stx *)xsrc)->srcloc->src;
    line = ((Scheme_Stx *)xsrc)->srcloc->line;
    col = ((Scheme_Stx *)xsrc)->srcloc->col;
    pos = ((Scheme_Stx *)xsrc)->srcloc->pos;

    if (show_loc)
      fn = make_srcloc_string(((Scheme_Stx *)xsrc)->srcloc, &fnlen);
    else
      fn = NULL;
  } else
    fn = NULL;

  if (!fn && show_loc) {
    intptr_t column;

    if (col < 0)
      column = pos;
    else
      column = col;

    if (port) {
      Scheme_Object *pn;
      pn = SCHEME_IPORT_NAME(port);
      if (SCHEME_PATHP(pn)) {
	pn = scheme_remove_current_directory_prefix(pn);
	fn = SCHEME_PATH_VAL(pn);
      } else
	fn = "UNKNOWN";
    } else
      fn = "UNKNOWN";

    fnlen = strlen(fn);

    if (column >= 0) {
      scheme_sprintf(lbuf, 30, ":%L%ld", line, column-1);
      ls = lbuf;
    } else
      ls = ": ";
  } else if (!show_loc) {
    fn = "";
    fnlen = 0;
  }

  if (indentation)
    suggests = scheme_extract_indentation_suggestions(indentation);
  else
    suggests = "";

  loc = scheme_make_location(stxsrc ? stxsrc : scheme_false,
			     (line < 0) ? scheme_false : scheme_make_integer(line),
			     (col < 0) ? scheme_false : scheme_make_integer(col-1),
			     (pos < 0) ? scheme_false : scheme_make_integer(pos),
			     (span < 0) ? scheme_false : scheme_make_integer(span));

  scheme_raise_exn(((gotc == EOF) 
		    ? MZEXN_FAIL_READ_EOF 
		    : ((gotc == SCHEME_SPECIAL) 
		       ? MZEXN_FAIL_READ_NON_CHAR 
		       : MZEXN_FAIL_READ)),
		   scheme_make_pair(loc, scheme_null),
		   "%t%s%s%t%s%s",
                   fn, fnlen, ls,
                   fnlen ? ": " : "",
		   s, slen, 
                   (*suggests ? "\n  possible cause: " : ""), suggests);
}

static void do_wrong_syntax(const char *where,
                            Scheme_Object *detail_form,
                            Scheme_Object *form,
                            char *s, intptr_t slen,
                            Scheme_Object *extra_sources,
                            int exn_kind)
{
  intptr_t len, vlen, dvlen, blen, plen;
  char *buffer;
  char *v, *dv, *p;
  Scheme_Object *mod, *nomwho, *who;
  int show_src;

  who = NULL;
  nomwho = NULL;
  mod = scheme_false;

  if (!s) {
    s = "bad syntax";
    slen = strlen(s);
  }

  /* Check for special strings that indicate `form' doesn't have a
     good name: */
  if ((where == scheme_compile_stx_string)
      || (where == scheme_expand_stx_string)) {
    where = NULL;
  } else if (where == scheme_application_stx_string) {
    who = scheme_intern_symbol("#%app");
    nomwho = who;
    mod = scheme_intern_symbol("racket");
  } else if ((where == scheme_set_stx_string)
	     || (where == scheme_var_ref_string)
	     || (where == scheme_begin_stx_string)) {
    who = scheme_intern_symbol(where);
    nomwho = who;
    mod = scheme_intern_symbol("racket");
    if (where == scheme_begin_stx_string)
      where = "begin (possibly implicit)";
  }

  buffer = init_buf(&len, &blen);

  p = NULL;
  plen = 0;

  show_src = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC));

  if (form) {
    Scheme_Object *pform;
    if (SCHEME_STXP(form)) {
      p = make_srcloc_string(((Scheme_Stx *)form)->srcloc, &plen);
      pform = scheme_syntax_to_datum(form, 0, NULL);

      /* Try to extract syntax name from syntax */
      if (!nomwho && (SCHEME_SYMBOLP(SCHEME_STX_VAL(form)) || SCHEME_STX_PAIRP(form))) {
	Scheme_Object *first;
	if (SCHEME_STX_PAIRP(form))
	  first = SCHEME_STX_CAR(form);
	else
	  first = form;
	if (SCHEME_SYMBOLP(SCHEME_STX_VAL(first))) {
	  /* Get module and name at source: */
	  int phase;
	  who = SCHEME_STX_VAL(first); /* printed name is local name */
	  /* name in exception is nominal source: */
 	  if (scheme_current_thread->current_local_env)
	    phase = scheme_current_thread->current_local_env->genv->phase;
	  else phase = 0;
	  scheme_stx_module_name(0, &first, scheme_make_integer(phase), &mod, &nomwho, 
                                 NULL, NULL, NULL, NULL, NULL, NULL, NULL);
	}
      }
    } else {
      pform = form;
      if (!detail_form)
	form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1, 0);
    }
    /* don't use error_write_to_string_w_max since this is code */
    if (show_src)
      v = scheme_write_to_string_w_max(pform, &vlen, len);
    else {
      v = NULL;
      vlen = 0;
    }
  } else {
    form = scheme_false;
    v = NULL;
    vlen = 0;
  }

  if (detail_form) {
    Scheme_Object *pform;
    if (SCHEME_STXP(detail_form)) {
      if (((Scheme_Stx *)detail_form)->srcloc->line >= 0)
	p = make_srcloc_string(((Scheme_Stx *)detail_form)->srcloc, &plen);
      pform = scheme_syntax_to_datum(detail_form, 0, NULL);
      /* To go in exn record: */
      form = detail_form;
    } else {
      pform = detail_form;
      /* To go in exn record: */
      form = scheme_datum_to_syntax(detail_form,
				    /* Use source location of `form': */
				    SCHEME_STXP(form) ? form : scheme_false,
				    scheme_false, 1, 0);
    }

    /* don't use error_write_to_string_w_max since this is code */
    if (show_src)
      dv = scheme_write_to_string_w_max(pform, &dvlen, len);
    else {
      dv = NULL;
      dvlen = 0;
    }
  } else {
    dv = NULL;
    dvlen = 0;
  }

  if (!who) {
    if (where)
      who = scheme_intern_symbol(where);
    else
      who = scheme_false;
  }
  if (!nomwho)
    nomwho = who;

  if (!where) {
    if (SCHEME_FALSEP(who))
      where = "?";
    else
      where = scheme_symbol_val(who);
  }
  
  if (v) {
    if (dv)
      blen = scheme_sprintf(buffer, blen, 
                            "%t%s%s: %t\n"
                            "  at: %t\n"
                            "  in: %t",
                            p, plen,
                            p ? ": " : "",
			    where,
                            s, slen,
			    dv, dvlen,
			    v, vlen);
    else
      blen = scheme_sprintf(buffer, blen, 
                            "%t%s%s: %t\n"
                            "  in: %t",
                            p, plen,
                            p ? ": " : "",
			    where,
                            s, slen,
			    v, vlen);
  } else
    blen = scheme_sprintf(buffer, blen, "%s: %t", 
                          where,
                          s, slen);

  /* We don't actually use nomwho and mod, anymore. */

  if (SCHEME_FALSEP(form))
    form = extra_sources;
  else {
    if (SCHEME_STXP(form))
      form = scheme_stx_taint(form);
    form = scheme_make_pair(form, extra_sources);
  }

  scheme_raise_exn(exn_kind, 
		   form,
		   "%t", buffer, blen);
}

void scheme_wrong_syntax(const char *where,
			 Scheme_Object *detail_form,
			 Scheme_Object *form,
			 const char *detail, ...)
{
  char *s;
  intptr_t slen;

  if (!detail) {
    s = NULL;
    slen = 0;
  } else {
    GC_CAN_IGNORE va_list args;

    HIDE_FROM_XFORM(va_start(args, detail));
    slen = sch_vsprintf(NULL, 0, detail, args, &s, NULL);
    HIDE_FROM_XFORM(va_end(args));
  }

  do_wrong_syntax(where, detail_form, form, s, slen, scheme_null, MZEXN_FAIL_SYNTAX);
}

void scheme_unbound_syntax(const char *where,
                           Scheme_Object *detail_form,
                           Scheme_Object *form,
                           const char *detail, ...)
{
  char *s;
  intptr_t slen;
  GC_CAN_IGNORE va_list args;

  HIDE_FROM_XFORM(va_start(args, detail));
  slen = sch_vsprintf(NULL, 0, detail, args, &s, NULL);
  HIDE_FROM_XFORM(va_end(args));

  do_wrong_syntax(where, detail_form, form, s, slen, scheme_null, MZEXN_FAIL_SYNTAX_UNBOUND);
}

void scheme_wrong_syntax_with_more_sources(const char *where,
                                           Scheme_Object *detail_form,
                                           Scheme_Object *form,
                                           Scheme_Object *extra_sources,
                                           const char *detail, ...)
{
  char *s;
  intptr_t slen;

  if (!detail) {
    s = NULL;
    slen = 0;
  } else {
    GC_CAN_IGNORE va_list args;

    HIDE_FROM_XFORM(va_start(args, detail));
    slen = sch_vsprintf(NULL, 0, detail, args, &s, NULL);
    HIDE_FROM_XFORM(va_end(args));
  }

  do_wrong_syntax(where, detail_form, form, s, slen, extra_sources, MZEXN_FAIL_SYNTAX);
}

void scheme_wrong_rator(Scheme_Object *rator, int argc, Scheme_Object **argv)
{
  intptr_t slen, rlen;
  char *s, *r;

  r = scheme_make_provided_string(rator, 1, &rlen);

  s = scheme_make_arg_lines_string("   ", -1, argc, argv, &slen);
    
  scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                   "application: not a procedure;\n"
                   " expected a procedure that can be applied to arguments\n"
                   "  given: %t\n"
                   "  arguments...:%t",
                   r, rlen, s, slen);
}

void scheme_wrong_return_arity(const char *where,
			       int expected, int got,
			       Scheme_Object **argv,
			       const char *detail, ...)
{
  intptr_t slen, vlen, blen;
  char *s, *buffer;
  char *v;

  if ((got != 1) && SAME_OBJ(scheme_current_thread->ku.multiple.array,
			     scheme_current_thread->values_buffer))
    scheme_current_thread->values_buffer = NULL;
  scheme_current_thread->ku.multiple.array = NULL;

  if (!detail) {
    s = NULL;
    slen = 0;
  } else {
    GC_CAN_IGNORE va_list args;

    HIDE_FROM_XFORM(va_start(args, detail));
    slen = sch_vsprintf(NULL, 0, detail, args, &s, NULL);
    HIDE_FROM_XFORM(va_end(args));
  }

  buffer = init_buf(NULL, &blen);

  if (!got || !argv) {
    v = "";
    vlen = 0;
  } else {
    Scheme_Object **array;

    array = ((got == 1) ? (Scheme_Object **) mzALIAS &argv : argv);

    v = scheme_make_arg_lines_string("   ", -1, got, array, &vlen);
  }

  blen = scheme_sprintf(buffer,
			blen,
			"%s%sresult arity mismatch;\n"
                        " expected number of values not received\n"
                        "  expected: %d\n"
			"  received: %d\n"
                        "%s%t%s"
                        "  values...:%t",
			where ? where : "",
			where ? ": " : "",
			expected,
			got,
                        slen ? "  from: " : "",
			s, slen,
                        slen ? "\n" : "",
			v, vlen);

  scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
		   "%t",
		   buffer, blen);
}

void scheme_non_fixnum_result(const char *name, Scheme_Object *o)
{
  scheme_raise_exn(MZEXN_FAIL_CONTRACT_NON_FIXNUM_RESULT,
                   "%s: result is not a fixnum\n"
                   "  result: %V",
                   name, o);
}

void scheme_raise_out_of_memory(const char *where, const char *msg, ...)
{
  char *s;
  intptr_t slen;

  if (!msg) {
    s = "";
    slen = 0;
  } else {
    GC_CAN_IGNORE va_list args;

    HIDE_FROM_XFORM(va_start(args, msg));
    slen = sch_vsprintf(NULL, 0, msg, args, &s, NULL);
    HIDE_FROM_XFORM(va_end(args));
  }

  scheme_raise_exn(MZEXN_FAIL_OUT_OF_MEMORY,
		   "%s%sout of memory %t",
		   where ? where : "",
		   where ? ": " : "",
		   s, slen);
}

void scheme_unbound_global(Scheme_Bucket *b)
{
  Scheme_Object *name = (Scheme_Object *)b->key;
  Scheme_Env *home;

  home = scheme_get_bucket_home(b);

  if (home && home->module) {
    const char *errmsg;
    char *phase, phase_buf[20], *phase_note = "";
    
    if (SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_SRCLOC)))
      errmsg = ("%S: undefined;\n"
                " cannot reference an identifier before its definition\n"
                "  in module: %D%s%s");
    else
      errmsg = ("%S: undefined;\n"
                " cannot reference an identifier before its definition%_%s%s");

    if (home->phase) {
      sprintf(phase_buf, "\n  phase: %" PRIdPTR "", home->phase);
      phase = phase_buf;
      if ((home->phase == 1) && (home->template_env)) {
        if (scheme_lookup_in_table(home->template_env->toplevel, (const char *)name))
          phase_note = "\n  explanation: cannot access the run-time definition";
        else if (home->template_env->syntax
                 && scheme_lookup_in_table(home->template_env->syntax, (const char *)name))
          phase_note = "\n  explanation cannot access the syntax binding for run-time expressions";
      }
    } else
      phase = "";

    scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE,
		     name,
		     errmsg,
		     name,
		     home->module->modsrc,
                     phase,
                     phase_note);
  } else {
    scheme_raise_exn(MZEXN_FAIL_CONTRACT_VARIABLE,
		     name,
		     "%S: undefined;\n"
                     " cannot reference undefined identifier",
		     name);
  }
}

char *scheme_make_provided_string(Scheme_Object *o, int count, intptr_t *lenout)
{
  intptr_t len;

  len = scheme_get_print_width();

  if (count)
    len /= count;

  return error_write_to_string_w_max(o, len, lenout);
}

static char *make_provided_list(Scheme_Object *o, int count, intptr_t *lenout)
{
  intptr_t len, cnt, i, onelen, total, sz;
  char *s, *accum, *naya;

  cnt = scheme_proper_list_length(o);
  if (cnt < 0)
    return scheme_make_provided_string(o, count, lenout);

  if (!cnt) {
    *lenout = 0;
    return "";
  }

  len = scheme_get_print_width();

  if (count)
    len /= count;

  total = 0;
  sz = 64;
  accum = (char *)scheme_malloc_atomic(sz);
  
  for (i = 0; i < cnt; i++) {
    s = scheme_write_to_string_w_max(SCHEME_CAR(o), &onelen, len / cnt);
    if (total + onelen + 1 >= sz) {
      sz = (2 * sz) + onelen + 1;
      naya = (char *)scheme_malloc_atomic(sz);
      memcpy(naya, accum, total);
      accum = naya;
    }
    memcpy(accum + total, s, onelen);
    accum[total + onelen] = ' ';
    total += onelen + 1;
    o = SCHEME_CDR(o);
  }

  total -= 1;
  accum[total] = 0;
  *lenout = total;

  return accum;
}

static Scheme_Object *do_error(const char *who, int mode, int argc, Scheme_Object *argv[])
{
  Scheme_Object *newargs[2];

  if (SCHEME_SYMBOLP(argv[0])) {
    if (argc < 2) {
      const char *s;
      int l;

      s = scheme_symbol_val(argv[0]);
      l = SCHEME_SYM_LEN(argv[0]);

      /* Just a symbol */
      newargs[0] =
	scheme_append_char_string(scheme_make_utf8_string("error: "),
				  scheme_make_sized_utf8_string((char *)s, l));
      
      SCHEME_SET_CHAR_STRING_IMMUTABLE(newargs[0]);
    } else {
      char *s, *r;
      intptr_t l, l2;
      Scheme_Object *port;
      port = scheme_make_byte_string_output_port();

      /* Chez-style: symbol, format string, format items... */
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_contract(who, "string?", 1, argc, argv);

      scheme_do_format(who, port, NULL, -1, 1, 2, argc, argv);

      s = scheme_get_sized_byte_string_output(port, &l);

      l2 = SCHEME_SYM_LEN(argv[0]);
      r = MALLOC_N_ATOMIC(char, l + l2 + 3);
      memcpy(r, SCHEME_SYM_VAL(argv[0]), l2);
      memcpy(r + l2, ": ", 2);
      memcpy(r + l2 + 2, s, l + 1);

      newargs[0] = scheme_make_immutable_sized_utf8_string(r, l + l2 + 2);
    }
  } else {
    Scheme_Object *strout;
    char *str;
    intptr_t len, i;

    /* String followed by other values: */
    if (!SCHEME_CHAR_STRINGP(argv[0]))
      scheme_wrong_contract(who, "(or/c string? symbol?)", 0, argc, argv);

    strout = scheme_make_byte_string_output_port();

    scheme_internal_display(argv[0], strout);
    for (i = 1; i < argc ; i++) {
      scheme_write_byte_string(" ", 1, strout);
      scheme_internal_write(argv[i], strout);
    }

    str = scheme_get_sized_byte_string_output(strout, &len);
    newargs[0] = scheme_make_immutable_sized_utf8_string(str, len);
  }

#ifndef NO_SCHEME_EXNS
  newargs[1] = TMP_CMARK_VALUE;
  do_raise(scheme_make_struct_instance(exn_table[mode].type,
				       2, newargs),
	   1,
           1);

  return scheme_void;
#else
  _scheme_apply_multi(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_DISPLAY_HANDLER), 1, newargs);

  return _scheme_tail_apply(scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_ESCAPE_HANDLER),
			    0, NULL);
#endif
}

static Scheme_Object *error(int argc, Scheme_Object *argv[])
{
  return do_error("error", MZEXN_FAIL, argc, argv);
}

static Scheme_Object *raise_user_error(int argc, Scheme_Object *argv[])
{
  return do_error("raise-user-error", MZEXN_FAIL_USER, argc, argv);
}

static Scheme_Object *raise_syntax_error(int argc, Scheme_Object *argv[])
{
  const char *who;
  Scheme_Object *str, *extra_sources = scheme_null;

  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract("raise-syntax-error", "(or/c symbol? #f)", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract("raise-syntax-error", "string?", 1, argc, argv);

  if (SCHEME_SYMBOLP(argv[0]))
    who = scheme_symbol_val(argv[0]);
  else
    who = NULL;

  str = argv[1];
  if (SCHEME_MUTABLEP(str)) {
    str = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(str), 
						  SCHEME_CHAR_STRLEN_VAL(str), 
						  1);
  }

  if (argc > 4) {
    extra_sources = argv[4];
    while (SCHEME_PAIRP(extra_sources)) {
      if (!SCHEME_STXP(SCHEME_CAR(extra_sources)))
        break;
      extra_sources = SCHEME_CDR(extra_sources);
    }
    if (!SCHEME_NULLP(extra_sources)) {
      scheme_wrong_contract("raise-syntax-error", "(listof syntax?)", 4, argc, argv);
      return NULL;
    }
    extra_sources = argv[4];
  }

  scheme_wrong_syntax_with_more_sources(who,
                                        ((argc > 3) && !SCHEME_FALSEP(argv[3])) ? argv[3] : NULL,
                                        ((argc > 2) && !SCHEME_FALSEP(argv[2])) ? argv[2] : NULL,
                                        extra_sources,
                                        "%T", str);

  return NULL;
}

typedef void (*wrong_proc_t)(const char *name, const char *expected,
                             int which, int argc,
                             Scheme_Object **argv);

static Scheme_Object *do_raise_type_error(const char *name, int argc, Scheme_Object *argv[], int mode)
{
  wrong_proc_t wrong;
  int negate = 0;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract(name, "symbol?", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract(name, "string?", 1, argc, argv);

  switch (mode) {
  case 0: wrong = scheme_wrong_type; break;
  case 1: wrong = scheme_wrong_contract; break;
  case 2: wrong = scheme_wrong_contract; negate = 1; break;
  default: wrong = NULL; break;
  }

  if (argc == 3) {
    Scheme_Object *v, *s;
    v = argv[2];
    s = scheme_char_string_to_byte_string(argv[1]);
    wrong(scheme_symbol_val(argv[0]),
          SCHEME_BYTE_STR_VAL(s),
          negate ? -2 : -1, 0, &v);
  } else {
    Scheme_Object **args, *s;
    int i;

    if (!(SCHEME_INTP(argv[2]) && (SCHEME_INT_VAL(argv[2]) >= 0))
	&& !(SCHEME_BIGNUMP(argv[2]) && SCHEME_BIGPOS(argv[2])))
      scheme_wrong_contract(name, "exact-nonnegative-integer?", 2, argc, argv);

    if ((SCHEME_INTP(argv[2]) && (SCHEME_INT_VAL(argv[2]) >= argc - 3))
	|| SCHEME_BIGNUMP(argv[2]))
      scheme_contract_error(name,
                            (negate
                             ? "position index >= provided result count"
                             : "position index >= provided argument count"),
                            "position index", 1, argv[2],
                            (negate ? "provided result count" : "provided argument count"), 
                            1, 
                            scheme_make_integer(argc - 3),
                            NULL);

    args = MALLOC_N(Scheme_Object *, argc - 3);
    for (i = 3; i < argc; i++) {
      args[i - 3] = argv[i];
    }

    s = scheme_char_string_to_byte_string(argv[1]);

    wrong(scheme_symbol_val(argv[0]),
          SCHEME_BYTE_STR_VAL(s),
          SCHEME_INT_VAL(argv[2]),
          negate ? (3 - argc) : (argc - 3), args);
  }

  return NULL;
}

static Scheme_Object *raise_type_error(int argc, Scheme_Object *argv[])
{
  return do_raise_type_error("raise-type-error", argc, argv, 0);
}

static Scheme_Object *raise_argument_error(int argc, Scheme_Object *argv[])
{
  return do_raise_type_error("raise-argument-error", argc, argv, 1);
}

static Scheme_Object *raise_result_error(int argc, Scheme_Object *argv[])
{
  return do_raise_type_error("raise-result-error", argc, argv, 2);
}

static Scheme_Object *do_raise_mismatch_error(const char *who, int mismatch, int argc, Scheme_Object *argv[])
{
  Scheme_Object *s;
  int i;
  char *s2;
  intptr_t l2;

  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_contract(who, "symbol?", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract(who, "string?", 1, argc, argv);

  /* additional arguments: alternate ones must be strings */
  for (i = 2 + mismatch; i < argc; i += 2) {
    if (!SCHEME_CHAR_STRINGP(argv[i]))
      scheme_wrong_contract(who, "string?", i, argc, argv);
  }

  if (!mismatch && (argc & 1)) {
    scheme_contract_error(who,
                          "missing value after field string",
                          "field string", 1, argv[argc-1],
                          NULL);
  }

  if (!mismatch && (argc == 2)) {
    /* Simple case: one string & value: */
    s = scheme_char_string_to_byte_string(argv[1]);
    
    scheme_contract_error(scheme_symbol_val(argv[0]),
                          SCHEME_BYTE_STR_VAL(s),
                          NULL);
  } else if (mismatch && (argc == 3)) {
    /* Simple case: one string & value: */
    s = scheme_char_string_to_byte_string(argv[1]);
    
    scheme_arg_mismatch(scheme_symbol_val(argv[0]),
                        SCHEME_BYTE_STR_VAL(s),
                        argv[2]);
  } else {
    /* Multiple strings & values: */
    char *st, **ss;
    intptr_t slen, *slens, total = 0;
    int offset = (mismatch ? 0 : 1);
    int scount = argc - 1 - offset;

    ss = (char **)MALLOC_N(char*, scount);
    slens = (intptr_t *)MALLOC_N_ATOMIC(intptr_t, scount);

    for (i = 1; (i + offset) < argc; i++) {
      if (i & 1) {
        s = scheme_char_string_to_byte_string(argv[i+offset]);
        st = SCHEME_BYTE_STR_VAL(s);
        slen = SCHEME_BYTE_STRLEN_VAL(s);
        if (!mismatch)
          total += 5;
      } else {
        st = scheme_make_provided_string(argv[i+offset], scount / 2, &slen);
      }
      total += slen;
      ss[i-1] = st;
      slens[i-1] = slen;
    }
    st = (char *)scheme_malloc_atomic(total + 1);

    total = 0;
    for (i = 0; i < scount; i++) {
      slen = slens[i];
      if (!mismatch && !(i & 1)) {
        memcpy(st + total, "\n   ", 3);
        total += 3;
      }
      memcpy(st + total, ss[i], slen);
      total += slen;
      if (!mismatch && !(i & 1)) {
        memcpy(st + total, ": ", 2);
        total += 2;
      }
    }
    st[total] = 0;

    s = scheme_char_string_to_byte_string(argv[1]);
    if (mismatch) {
      s2 = "";
      l2 = 0;
    } else {
      s2 = SCHEME_BYTE_STR_VAL(s);
      l2 = SCHEME_BYTE_STRLEN_VAL(s);
    }
    
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                     "%s: %t%t",
                     scheme_symbol_val(argv[0]), 
                     s2, l2,
                     st, total);
  }

  return NULL;
}

static Scheme_Object *raise_mismatch_error(int argc, Scheme_Object *argv[])
{
  return do_raise_mismatch_error("raise-mismatch-error", 1, argc, argv);
}

static Scheme_Object *raise_arguments_error(int argc, Scheme_Object *argv[])
{
  return do_raise_mismatch_error("raise-arguments-error", 0, argc, argv);
}


static int is_arity_at_least(Scheme_Object *v)
{
  return (SCHEME_CHAPERONE_STRUCTP(v)
          && scheme_is_struct_instance(scheme_arity_at_least, v)
          && scheme_nonneg_exact_p(((Scheme_Structure *)v)->slots[0]));
}

static int is_arity_list(Scheme_Object *l)
{
  int c;
  Scheme_Object *a;

  c = scheme_proper_list_length(l);
  if (c < 0) return 0;
  while (!SCHEME_NULLP(l)) {
    a = SCHEME_CAR(l);
    if (!scheme_nonneg_exact_p(a)
        && !scheme_nonneg_exact_p(a))
      return 0;
    l = SCHEME_CDR(l);
  }

  return 1;
}

static Scheme_Object *raise_arity_error(int argc, Scheme_Object *argv[])
{
  Scheme_Object **args;
  const char *name;
  int minc, maxc;

  if (!SCHEME_SYMBOLP(argv[0]) && !SCHEME_PROCP(argv[0]))
    scheme_wrong_contract("raise-arity-error", "(or/c symbol? procedure?)", 0, argc, argv);
  if (!scheme_nonneg_exact_p(argv[1]) 
      && !is_arity_at_least(argv[1])
      && !is_arity_list(argv[1]))
    scheme_wrong_contract("raise-mismatch-error", 
                          "(or/c exact-nonnegative-integer? arity-at-least? (listof (or/c exact-nonnegative-integer? arity-at-least?)))", 
                          1, argc, argv);

  args = MALLOC_N(Scheme_Object*, argc - 2);
  memcpy(args, argv + 2, sizeof(Scheme_Object*) * (argc - 2));

  if (SCHEME_SYMBOLP(argv[0]))
    name = scheme_symbol_val(argv[0]);
  else {
    int len;
    name = scheme_get_proc_name(argv[0], &len, 1);
  }

  if (SCHEME_INTP(argv[1])) {
    minc = maxc = SCHEME_INT_VAL(argv[1]);
  } else if (is_arity_at_least(argv[1])) {
    Scheme_Object *v;
    v = scheme_struct_ref(argv[1], 0);
    if (SCHEME_INTP(v)) {
      minc = SCHEME_INT_VAL(v);
      maxc = -1;
    } else {
      minc = -2;
      maxc = 0;
    }
  } else {
    minc = -2;
    maxc = 0;
  }

  scheme_wrong_count_m(name, minc, maxc, argc - 2, args, 0);

  return NULL;
}

static Scheme_Object *good_print_width(int c, Scheme_Object **argv)
{
  int ok;

  ok = (SCHEME_INTP(argv[0]) 
	? (SCHEME_INT_VAL(argv[0]) > 3)
	: (SCHEME_BIGNUMP(argv[0])
	   ? SCHEME_BIGPOS(argv[0])
	   : 0));

  return ok ? scheme_true : scheme_false;
}

static Scheme_Object *error_print_width(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-print-width",
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_WIDTH),
			     argc, argv,
			     -1, good_print_width, "exact integer greater than three", 0);
}

static Scheme_Object *good_print_context_length(int c, Scheme_Object **argv)
{
  int ok;

  ok = (SCHEME_INTP(argv[0]) 
	? (SCHEME_INT_VAL(argv[0]) >= 0)
	: (SCHEME_BIGNUMP(argv[0])
	   ? SCHEME_BIGPOS(argv[0])
	   : 0));

  return ok ? scheme_true : scheme_false;
}

static Scheme_Object *error_print_context_length(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-print-context-length",
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_CONTEXT_LENGTH),
			     argc, argv,
			     -1, good_print_context_length, "non-negative integer", 0);
}

static Scheme_Object *error_print_srcloc(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-print-source-location",
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_SRCLOC),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

void scheme_write_proc_context(Scheme_Object *port, int print_width,
                               Scheme_Object *name, 
                               Scheme_Object *src, Scheme_Object *line, 
                               Scheme_Object *col, Scheme_Object *pos,
                               int generated)
{
  if (src) {
    scheme_display_w_max(src, port, print_width);
    if (line && SCHEME_TRUEP(line)) {
      /* Line + column */
      scheme_write_byte_string(":", 1, port);
      scheme_display_w_max(line, port, print_width);
      scheme_write_byte_string(":", 1, port);
      scheme_display_w_max(col, port, print_width);
    } else if (pos && SCHEME_TRUEP(pos)) {
      /* Position */
      scheme_write_byte_string("::", 2, port);
      scheme_display_w_max(pos, port, print_width);
    }
    
    if (SCHEME_TRUEP(name)) {
      scheme_write_byte_string(": ", 2, port);
    }
  }
  
  if (SCHEME_TRUEP(name)) {
    scheme_display_w_max(name, port, print_width);
  }
}

static Scheme_Object *
def_error_display_proc(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config;
  Scheme_Object *port, *s;

  config = scheme_current_config();
  port = scheme_get_param(config, MZCONFIG_ERROR_PORT);

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("default-error-display-handler", "string?", 0, argc, argv);
  /* don't care about argv[1] */

  s = scheme_char_string_to_byte_string(argv[0]);

  scheme_write_byte_string(SCHEME_BYTE_STR_VAL(s),
			   SCHEME_BYTE_STRTAG_VAL(s),
			   port);

  /* Print context, if available */
  if (SCHEME_CHAPERONE_STRUCTP(argv[1])
      && scheme_is_struct_instance(exn_table[MZEXN].type, argv[1])
      && !scheme_is_struct_instance(exn_table[MZEXN_FAIL_USER].type, argv[1])) {
    Scheme_Object *l, *w;
    int print_width = 1024, max_cnt = 16;

    w = scheme_get_param(config, MZCONFIG_ERROR_PRINT_CONTEXT_LENGTH);
    if (SCHEME_INTP(w))
      max_cnt = SCHEME_INT_VAL(w);
    else
      max_cnt = 0x7FFFFFFF;

    if (max_cnt) {
      int orig_max_cnt = max_cnt;
      w = scheme_get_param(config, MZCONFIG_ERROR_PRINT_WIDTH);
      if (SCHEME_INTP(w))
	print_width = SCHEME_INT_VAL(w);
      else
	print_width = 0x7FFFFFFF;
      l = scheme_get_stack_trace(scheme_struct_ref(argv[1], 1));
      while (!SCHEME_NULLP(l)) {
	if (!max_cnt) {
	  scheme_write_byte_string("...\n", 4, port);
	  break;
	} else {
	  Scheme_Object *name, *loc;
	  
	  if (max_cnt == orig_max_cnt) {
	    /* Starting label: */
	    scheme_write_byte_string("\n  context...:\n", 15, port);
	  } else
            scheme_write_byte_string("\n", 1, port);

	  name = SCHEME_CAR(l);
	  loc = SCHEME_CDR(name);
	  name = SCHEME_CAR(name);

          scheme_write_byte_string("   ", 3, port);

          if (SCHEME_TRUEP(loc)) {
            Scheme_Structure *sloc = (Scheme_Structure *)loc;
            scheme_write_proc_context(port, print_width, 
                                      name, 
                                      sloc->slots[0], sloc->slots[1],
                                      sloc->slots[2], sloc->slots[3],
                                      0);
          } else {
            scheme_write_proc_context(port, print_width, 
                                      name, 
                                      NULL, NULL, NULL, NULL, 
                                      0);
          }

	  l = SCHEME_CDR(l);
	  --max_cnt;
	}
      }
    }
  }

  scheme_write_byte_string("\n", 1, port);

  return scheme_void;
}

static Scheme_Object *
emergency_error_display_proc(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    return scheme_void;

  s = scheme_char_string_to_byte_string(argv[0]);

  scheme_log_message(NULL, SCHEME_LOG_ERROR, 
                     SCHEME_BYTE_STR_VAL(s), SCHEME_BYTE_STRTAG_VAL(s), 
                     scheme_false);

  return scheme_void;
}

static Scheme_Object *
def_error_value_string_proc(int argc, Scheme_Object *argv[])
{
  intptr_t origl, len, l;
  char *s;
  Scheme_Object *pph;

  if (!SCHEME_INTP(argv[1]))
    scheme_wrong_contract("default-error-value->string-handler", "number?", 1, argc, argv);

  origl = len = SCHEME_INT_VAL(argv[1]);

  pph = scheme_get_param(scheme_current_config(), MZCONFIG_PORT_PRINT_HANDLER);
  if (SAME_OBJ(pph, scheme_default_global_print_handler)) {
    if (len < 3)
      len = 3;

    s = scheme_print_to_string_w_max(argv[0], &l, len);

    if ((origl < 3) && (l > origl))
      l = origl;
  } else {
    Scheme_Object *a[2];

    a[0] = argv[0];
    a[1] = scheme_make_byte_string_output_port();
    _scheme_apply(pph, 2, a);

    s = scheme_get_sized_byte_string_output(a[1], &l);

    if (l > origl) {
      /* FIXME: might hit the middle of a UTF-8 encoding. */
      l = origl;
      if (origl >= 1) {
	s[origl - 1] = '.';
	if (origl >= 2) {
	  s[origl - 2] = '.';
	  if (origl >= 3)
	    s[origl - 3] = '.';
	}
      }
    }
  }

  return scheme_make_sized_utf8_string(s, l);
}

static Scheme_Object *
def_error_escape_proc(int argc, Scheme_Object *argv[])
{  
  Scheme_Object *prompt;
  Scheme_Thread *p = scheme_current_thread;

  prompt = scheme_extract_one_cc_mark(NULL, SCHEME_PTR_VAL(scheme_default_prompt_tag));

  if (prompt) {
    p->cjs.jumping_to_continuation = prompt;
    p->cjs.alt_full_continuation = NULL;
    p->cjs.num_vals = 1;
    p->cjs.val = scheme_void_proc;
  }
  scheme_longjmp(scheme_error_buf, 1);

  return scheme_void; /* Never get here */
}

static Scheme_Object *
error_display_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-display-handler",
			     scheme_make_integer(MZCONFIG_ERROR_DISPLAY_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
error_value_string_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-value->string-handler",
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_VALUE_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
error_escape_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-escape-handler",
			     scheme_make_integer(MZCONFIG_ERROR_ESCAPE_HANDLER),
			     argc, argv,
			     0, NULL, NULL, 0);
}

static Scheme_Object *
exit_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("exit-handler",
			     scheme_make_integer(MZCONFIG_EXIT_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
def_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  intptr_t status;

  if (SCHEME_INTP(argv[0])) {
    status = SCHEME_INT_VAL(argv[0]);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  if (scheme_exit)
    scheme_exit(status);
  else
    exit(status);

  return scheme_void;
}

Scheme_Object *
scheme_do_exit(int argc, Scheme_Object *argv[])
{
  intptr_t status;
  Scheme_Object *handler;

  if (argc == 1) {
    if (SCHEME_INTP(argv[0]))
      status = SCHEME_INT_VAL(argv[0]);
    else
      status = 0;
  } else
    status = 0;

  handler = scheme_get_param(scheme_current_config(), MZCONFIG_EXIT_HANDLER);

  if (handler) {
    Scheme_Object *p[1];

    p[0] = argc ? argv[0] : scheme_make_integer(status);
    scheme_apply_multi(handler, 1, p);
  } else if (scheme_exit)
    scheme_exit(status);
  else
    exit(status);

  return scheme_void;
}

/* scheme_immediate_exit ensures that a call to exit() goes to the C
   library used by the Racket DLL, and not some other copy of the
   library (in Windows) */
void scheme_immediate_exit(int status)
{
  exit(status);
}

static Scheme_Object *
exe_yield_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("exeuctable-yield-handler",
			     scheme_make_integer(MZCONFIG_EXE_YIELD_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *default_yield_handler(int argc, Scheme_Object **argv)
{
  return scheme_void;
}

/***********************************************************************/

static int extract_spec_level(Scheme_Object *level_spec, Scheme_Object *name)
{
  if (!level_spec) return 0;

  while (1) {
    if (SCHEME_INTP(level_spec))
      return SCHEME_INT_VAL(level_spec);
    else if (name && SAME_OBJ(SCHEME_CADR(level_spec), name))
      return SCHEME_INT_VAL(SCHEME_CAR(level_spec));
    level_spec = SCHEME_CDR(SCHEME_CDR(level_spec));
  }
}

void update_want_level(Scheme_Logger *logger)
{
  Scheme_Log_Reader *lr;
  Scheme_Object *stack = NULL, *queue, *b, *prev;
  Scheme_Logger *parent = logger;
  int want_level, level;

  while (parent) {
    stack = scheme_make_raw_pair((Scheme_Object *)parent, stack);
    parent = parent->parent;
  }

  want_level = 0;
  while (stack) {
    parent = (Scheme_Logger *)SCHEME_CAR(stack);
    
    queue = parent->readers;
    prev = NULL;
    while (queue) {
      b = SCHEME_CAR(queue);
      b = SCHEME_CAR(b);
      lr = (Scheme_Log_Reader *)SCHEME_BOX_VAL(b);
      if (lr) {
        level = extract_spec_level(lr->level, logger->name);
        if (level > want_level)
          want_level = level;
        prev = queue;
      } else {
        if (prev)
          SCHEME_CDR(prev) = SCHEME_CDR(queue);
        else
          parent->readers = SCHEME_CDR(queue);
      }
      queue = SCHEME_CDR(queue);
    }

    level = extract_spec_level(parent->syslog_level, logger->name);
    if (level > want_level)
      want_level = level;
    level = extract_spec_level(parent->stderr_level, logger->name);
    if (level > want_level)
      want_level = level;    

    stack = SCHEME_CDR(stack);
  }

  logger->want_level = want_level;
  logger->local_timestamp = *logger->timestamp;
}

#ifdef USE_WINDOWS_EVENT_LOG
static int event_procs_ready;
typedef HANDLE (WINAPI *mzRegisterEventSourceProc)(LPCTSTR lpUNCServerName, LPCTSTR lpSourceName);
typedef BOOL (WINAPI *mzReportEventProc)(HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID,
						                 PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, LPCTSTR* lpStrings,
							             LPVOID lpRawData);
static mzRegisterEventSourceProc mzRegisterEventSource;
static mzReportEventProc mzReportEvent;
#endif



static Scheme_Object *make_log_message(int level, Scheme_Object *name, 
                                       char *buffer, intptr_t len, Scheme_Object *data) {
  Scheme_Object *msg;
  Scheme_Object *v;
  
  msg = scheme_make_vector(4, NULL);
  switch (level) {
  case SCHEME_LOG_FATAL:
    v = fatal_symbol;
    break;
  case SCHEME_LOG_ERROR:
    v = error_symbol;
    break;
  case SCHEME_LOG_WARNING:
    v = warning_symbol;
    break;
  case SCHEME_LOG_INFO:
    v = info_symbol;
    break;
  case SCHEME_LOG_DEBUG:
  default:
    v = debug_symbol;
    break;
  }
  SCHEME_VEC_ELS(msg)[0] = v;
          
  if (name) {
    /* Add logger name prefix: */
    intptr_t slen;
    char *cp;
    slen = SCHEME_SYM_LEN(name);
    cp = scheme_malloc_atomic(slen + 2 + len + 1);
    memcpy(cp, SCHEME_SYM_VAL(name), slen);
    memcpy(cp + slen, ": ", 2);
    memcpy(cp + slen + 2, buffer, len + 1);
    len += slen + 2;
    buffer = cp;
  }

  v = scheme_make_sized_utf8_string(buffer, len);
  SCHEME_SET_CHAR_STRING_IMMUTABLE(v);
  SCHEME_VEC_ELS(msg)[1] = v;
  SCHEME_VEC_ELS(msg)[2] = (data ? data : scheme_false);
  SCHEME_VEC_ELS(msg)[3] = (name ? name : scheme_false);

  SCHEME_SET_VECTOR_IMMUTABLE(msg);

  return msg;
}

void scheme_log_name_message(Scheme_Logger *logger, int level, Scheme_Object *name, 
                             char *buffer, intptr_t len, Scheme_Object *data)
{
  /* This function must avoid GC allocation when called with the
     configuration of scheme_log_abort(). */
  Scheme_Object *queue, *q, *msg = NULL, *b;
  Scheme_Log_Reader *lr;
  Scheme_Logger *lo;

  if (!logger) {
    Scheme_Config *config;
    config = scheme_current_config();
    logger = (Scheme_Logger *)scheme_get_param(config, MZCONFIG_LOGGER);
  }

  if (logger->local_timestamp < *logger->timestamp)
    update_want_level(logger);

  if (logger->want_level < level)
    return;

  if (!name)
    name = logger->name;

  /* run notification callbacks: */
  for (lo = logger; lo; lo = lo->parent) {
    if (lo->callback) {
      Scheme_Object *a[1];
      if (!msg)
        msg = make_log_message(level, name, buffer, len, data);

      a[0] = msg;
      scheme_apply_multi(lo->callback, 1, a);
    }
  }

  if (SCHEME_FALSEP(name))
    name = NULL;

  while (logger) {
    if (extract_spec_level(logger->syslog_level, name) >= level) {
#ifdef USE_C_SYSLOG
      int pri;
      switch (level) {
      case SCHEME_LOG_FATAL:
        pri = LOG_CRIT;
        break;
      case SCHEME_LOG_ERROR:
        pri = LOG_ERR;
        break;
      case SCHEME_LOG_WARNING:
        pri = LOG_WARNING;
        break;
      case SCHEME_LOG_INFO:
        pri = LOG_INFO;
        break;
      case SCHEME_LOG_DEBUG:
      default:
        pri = LOG_DEBUG;
        break;
      }
      if (name)
        syslog(pri, "%s: %s", SCHEME_SYM_VAL(name), buffer);
      else
        syslog(pri, "%s", buffer);
#endif
#ifdef USE_WINDOWS_EVENT_LOG
      if (!event_procs_ready) {
        HMODULE hm;
        hm = LoadLibrary("advapi32.dll");
        if (hm) {
          mzRegisterEventSource = (mzRegisterEventSourceProc)GetProcAddress(hm, "RegisterEventSourceA");
          mzReportEvent = (mzReportEventProc)GetProcAddress(hm, "ReportEventA");
        }
        event_procs_ready = 1;
      }
      if (mzRegisterEventSource) {
        static HANDLE hEventLog;
        WORD ty;
        unsigned long sev;
        LPCTSTR a[1];

        if (!hEventLog) {
          Scheme_Object *cmd;
          cmd = scheme_get_run_cmd();
          hEventLog = mzRegisterEventSource(NULL, SCHEME_PATH_VAL(cmd));
        }

        switch (level) {
        case SCHEME_LOG_FATAL:
          ty = EVENTLOG_ERROR_TYPE;
          sev = 3;
          break;
        case SCHEME_LOG_ERROR:
          ty = EVENTLOG_ERROR_TYPE;
          sev = 3;
          break;
        case SCHEME_LOG_WARNING:
          ty = EVENTLOG_WARNING_TYPE;
          sev = 2;
          break;
        case SCHEME_LOG_INFO:
          ty = EVENTLOG_INFORMATION_TYPE;
          sev = 1;
          break;
        case SCHEME_LOG_DEBUG:
        default:
          ty = EVENTLOG_AUDIT_SUCCESS;
          sev = 0;
          break;
        }
        if (name) {
          char *naya;
          intptr_t slen;
          slen = SCHEME_SYM_LEN(name);
          naya = (char *)scheme_malloc_atomic(slen + 2 + len + 1);
          memcpy(naya, SCHEME_SYM_VAL(name), slen);
          memcpy(naya + slen, ": ", 2);
          memcpy(naya + slen + 2, buffer, len);
          naya[slen + 2 + len] = 0;
          buffer = naya;
          len += slen + 2;
        }
        a[0] = buffer;
        mzReportEvent(hEventLog, ty, 1 /* category */,
                      (sev << 30) | 2 /* message */,
                      NULL, 
                      1, 0,
                      a, NULL);
      }
#endif
    }
    if (extract_spec_level(logger->stderr_level, name) >= level) {
      if (name) {
        intptr_t slen;
        slen = SCHEME_SYM_LEN(name);
        fwrite(SCHEME_SYM_VAL(name), slen, 1, stderr);
        fwrite(": ", 2, 1, stderr);
      }
      fwrite(buffer, len, 1, stderr);
      fwrite("\n", 1, 1, stderr);
    }
    
    queue = logger->readers;
    while (queue) {
      b = SCHEME_CAR(queue);
      b = SCHEME_CAR(b);
      lr = (Scheme_Log_Reader *)SCHEME_BOX_VAL(b);
      if (lr) {
        if (extract_spec_level(lr->level, name) >= level) {
          if (!msg)
            msg = make_log_message(level, name, buffer, len, data);
          
          /* enqueue */
          q = scheme_make_raw_pair(msg, NULL);
          if (lr->tail)
            SCHEME_CDR(lr->tail) = q;
          else
            lr->head = q;
          lr->tail = q;
          scheme_post_sema(lr->sema);
        }
      }
      queue = SCHEME_CDR(queue);
    }

    logger = logger->parent;
  }
}

void scheme_log_message(Scheme_Logger *logger, int level, char *buffer, intptr_t len, Scheme_Object *data)
{
  scheme_log_name_message(logger, level, NULL, buffer, len, data);
}

void scheme_log_abort(char *buffer)
{
  Scheme_Logger logger;
  intptr_t ts;

  memset(&logger, 0, sizeof(logger));

  logger.name = NULL;
  logger.parent = NULL;
  logger.want_level = SCHEME_LOG_FATAL;

  ts = 0;
  logger.timestamp = &ts;
  logger.local_timestamp = ts;
  logger.syslog_level = init_syslog_level;
  logger.stderr_level = init_stderr_level;

  scheme_log_message(&logger, SCHEME_LOG_FATAL, buffer, strlen(buffer), scheme_false);
}

void scheme_log_warning(char *buffer)
{
  scheme_log_message(scheme_main_logger, SCHEME_LOG_WARNING, buffer, strlen(buffer), scheme_false);
}

void scheme_glib_log_message(const char *log_domain,
                             int log_level,
                             const char *message,
                             void *user_data)
/* This handler is suitable for use as a glib logging handler.
   Although a handler can be implemented with the FFI,
   we build one into Racket to avoid potential problems of
   handlers getting GCed or retaining a namespace. */
{
#define mzG_LOG_LEVEL_ERROR    (1 << 2)
#define mzG_LOG_LEVEL_CRITICAL (1 << 3)
#define mzG_LOG_LEVEL_WARNING  (1 << 4)
#define mzG_LOG_LEVEL_MESSAGE  (1 << 5)
#define mzG_LOG_LEVEL_INFO     (1 << 6)
#define mzG_LOG_LEVEL_DEBUG    (1 << 7)
  int level, len1, len2;
  char *together;

  if (log_level & (mzG_LOG_LEVEL_ERROR))
    level = SCHEME_LOG_FATAL;
  else if (log_level & (mzG_LOG_LEVEL_CRITICAL))
    level = SCHEME_LOG_ERROR;
  else if (log_level & (mzG_LOG_LEVEL_WARNING | mzG_LOG_LEVEL_MESSAGE))
    level = SCHEME_LOG_WARNING;
  else if (log_level & (mzG_LOG_LEVEL_INFO))
    level = SCHEME_LOG_INFO;
  else /* if (log_level & (mzG_LOG_LEVEL_DEBUG)) */
    level = SCHEME_LOG_DEBUG;

  len2 = strlen(message);
  if (log_domain) {
    len1 = strlen(log_domain);
    together = (char *)scheme_malloc_atomic(len1 + len2 + 3);
    memcpy(together, log_domain, len1);
    memcpy(together + len1, ": ", 2);
    memcpy(together + len1 + 2, message, len2);
    len2 += len1 + 2;
  } else
    together = (char *)message;
  
  scheme_log_message(scheme_main_logger, level, together, len2, scheme_false);
}

static int extract_level(const char *who, int none_ok, int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *v;
  int level;

  v = argv[which];
  if (SAME_OBJ(v, none_symbol))
    level = 0;
  else if (SAME_OBJ(v, fatal_symbol))
    level = SCHEME_LOG_FATAL;
  else if (SAME_OBJ(v, error_symbol))
    level = SCHEME_LOG_ERROR;
  else if (SAME_OBJ(v, warning_symbol))
    level = SCHEME_LOG_WARNING;
  else if (SAME_OBJ(v, info_symbol))
    level = SCHEME_LOG_INFO;
  else if (SAME_OBJ(v, debug_symbol))
    level = SCHEME_LOG_DEBUG;
  else {
    scheme_wrong_contract(who, 
                          (none_ok 
                           ? "(or/c 'none 'fatal 'error 'warning 'info 'debug)"
                           : "(or/c 'fatal 'error 'warning 'info 'debug)"),
                          which, argc, argv);
    return 0;
  }
  
  return level;
}

static Scheme_Object *
log_message(int argc, Scheme_Object *argv[])
{
  Scheme_Logger *logger;
  Scheme_Object *bytes;
  Scheme_Object *name;
  int level, pos;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type))
    scheme_wrong_contract("log-message", "logger?", 0, argc, argv);
  logger = (Scheme_Logger *)argv[0];

  level = extract_level("log-message", 0, 1, argc, argv);

  pos = 2;
  if (SCHEME_SYMBOLP(argv[pos]) || SCHEME_FALSEP(argv[pos]))
    name = argv[pos++];
  else
    name = NULL;

  bytes = argv[pos];
  if (!SCHEME_CHAR_STRINGP(bytes))
    scheme_wrong_contract("log-message", "string?", pos, argc, argv);
  bytes = scheme_char_string_to_byte_string(bytes);
  pos++;
  
  scheme_log_name_message(logger, level, name, SCHEME_BYTE_STR_VAL(bytes), SCHEME_BYTE_STRLEN_VAL(bytes), argv[pos]);

  return scheme_void;
}

static Scheme_Object *
log_level_p(int argc, Scheme_Object *argv[])
{
  Scheme_Logger *logger;
  int level;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type))
    scheme_wrong_contract("log-level?", "logger?", 0, argc, argv);
  logger = (Scheme_Logger *)argv[0];

  level = extract_level("log-level?", 0, 1, argc, argv);

  if (logger->local_timestamp < *logger->timestamp)
    update_want_level(logger);

  return ((logger->want_level >= level) ? scheme_true : scheme_false);
}

static Scheme_Object *
log_max_level(int argc, Scheme_Object *argv[])
{
  Scheme_Logger *logger;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type))
    scheme_wrong_contract("log-max-level", "logger?", 0, argc, argv);
  logger = (Scheme_Logger *)argv[0];

  if (logger->local_timestamp < *logger->timestamp)
    update_want_level(logger);

  switch (logger->want_level) {
  case 0:
    return scheme_false;
  case SCHEME_LOG_FATAL:
    return fatal_symbol;
  case SCHEME_LOG_ERROR:
    return error_symbol;
  case SCHEME_LOG_WARNING:
    return warning_symbol;
  case SCHEME_LOG_INFO:
    return info_symbol;
  default:
  case SCHEME_LOG_DEBUG:
    return debug_symbol;
  }
}

static Scheme_Object *
make_logger(int argc, Scheme_Object *argv[])
{
  Scheme_Logger *parent, *logger;

  if (argc) {
    if (!SCHEME_FALSEP(argv[0]) && !SCHEME_SYMBOLP(argv[0]))
      scheme_wrong_contract("make-logger", "(or/c symbol? #f)", 0, argc, argv);

    if (argc > 1) {
      if (SCHEME_FALSEP(argv[1]))
        parent = NULL;
      else {
        if (!SAME_TYPE(SCHEME_TYPE(argv[1]), scheme_logger_type))
          scheme_wrong_contract("make-logger", "(or/c logger? #f)", 1, argc, argv);
        parent = (Scheme_Logger *)argv[1];
      }

      if (argc > 2)
        (void)scheme_check_proc_arity2("make-logger", 1, 2, argc, argv, 1);
    } else
      parent = NULL;
  } else
    parent = NULL;

  logger = scheme_make_logger(parent, 
                              (argc 
                               ? (SCHEME_FALSEP(argv[0]) ? NULL : argv[0])
                               : NULL));

  if ((argc > 2) && SCHEME_TRUEP(argv[2]))
    logger->callback = argv[2];
  
  return (Scheme_Object *)logger;
}

Scheme_Logger *scheme_make_logger(Scheme_Logger *parent, Scheme_Object *name)
{
  Scheme_Logger *logger;

  logger = MALLOC_ONE_TAGGED(Scheme_Logger);
  logger->so.type = scheme_logger_type;
  logger->parent = parent;
  if (parent) {
    logger->timestamp = parent->timestamp;
  } else {
    intptr_t *timestamp;
    timestamp = MALLOC_ONE_ATOMIC(intptr_t);
    *timestamp = 1;
    logger->timestamp = timestamp;
  }
  logger->name = name;

  return logger;
}

static Scheme_Object *
logger_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *
current_logger(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-logger",
			     scheme_make_integer(MZCONFIG_LOGGER),
			     argc, argv,
			     -1, logger_p, "logger", 0);
}

static Scheme_Object *
logger_name(int argc, Scheme_Object *argv[])
{
  Scheme_Object *name;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type))
    scheme_wrong_contract("logger-name", "logger?", 0, argc, argv);

  name = ((Scheme_Logger *)argv[0])->name;
  return (name ? name : scheme_false);
}

static Scheme_Object *
make_log_reader(int argc, Scheme_Object *argv[])
{
  Scheme_Logger *logger;
  Scheme_Log_Reader *lr;
  Scheme_Object *sema, *q;
  int default_lvl = 0, lvl, i;
  Scheme_Object *level = scheme_null, *last = NULL;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_logger_type))
    scheme_wrong_contract("make-log-receiver", "logger?", 0, argc, argv);
  logger = (Scheme_Logger *)argv[0];

  for (i = 1; i < argc; i += 2) {
    lvl = extract_level("make-log-receiver", 1, i, argc, argv);
    if ((i+1) < argc) {
      if (SCHEME_FALSEP(argv[i+1]))
        default_lvl = lvl;
      else {
        if (!SCHEME_SYMBOLP(argv[i+1]))
          scheme_wrong_contract("make-log-receiver", "(or/c symbol? #f)", i+1, argc, argv);
        level = scheme_make_pair(argv[i+1], level);
        if (!last) last = level;
        level = scheme_make_pair(scheme_make_integer(lvl), level);
      }
    } else {
      default_lvl = lvl;
    }
  }

  if (last)
    SCHEME_CDR(last) = scheme_make_integer(default_lvl);
  else
    level = scheme_make_integer(default_lvl);

  lr = MALLOC_ONE_TAGGED(Scheme_Log_Reader);
  lr->so.type = scheme_log_reader_type;
  lr->level = level;

  sema = scheme_make_sema(0);
  lr->sema = sema;

  /* Pair a weak reference to the reader with a strong reference to the
     channel. Channel gets are wrapped to reference the reader. That way,
     the link is effectively strong while a thread is sync'd on the
     reader. */

  q = scheme_make_raw_pair(scheme_make_pair(scheme_make_weak_box((Scheme_Object *)lr), 
                                            sema),
                           logger->readers);
  logger->readers = q;
  *logger->timestamp += 1;

  return (Scheme_Object *)lr;
}

static Scheme_Object *
log_reader_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_log_reader_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *dequeue_log(Scheme_Object *_lr)
{
  Scheme_Log_Reader *lr = (Scheme_Log_Reader *)_lr;

  if (lr->head) {
    Scheme_Object *v;
    v = SCHEME_CAR(lr->head);
    lr->head = SCHEME_CDR(lr->head);
    if (!lr->head)
      lr->tail = NULL;
    return v;
  } else {
    scheme_signal_error("empty log-reader queue!?");
    return NULL;
  }
}

static int log_reader_get(Scheme_Object *_lr, Scheme_Schedule_Info *sinfo)
{
  Scheme_Log_Reader *lr = (Scheme_Log_Reader *)_lr;
  scheme_set_sync_target(sinfo, lr->sema, (Scheme_Object *)lr, NULL, 0, 1, dequeue_log);
  return 0;
}

/***********************************************************************/

void
scheme_raise_exn(int id, ...)
{
  GC_CAN_IGNORE va_list args;
  intptr_t alen;
  char *msg;
  int i, c;
  Scheme_Object *eargs[MZEXN_MAXARGS], *errno_val = NULL;
  char *buffer;

  /* Precise GC: Don't allocate before getting hidden args off stack */
  HIDE_FROM_XFORM(va_start(args, id));

  if (id == MZEXN_OTHER)
    c = 3;
  else
    c = exn_table[id].args;

  for (i = 2; i < c; i++) {
    eargs[i] = mzVA_ARG(args, Scheme_Object*);
  }

  msg = mzVA_ARG(args, char*);

  alen = sch_vsprintf(NULL, 0, msg, args, &buffer, &errno_val);
  HIDE_FROM_XFORM(va_end(args));

#ifndef NO_SCHEME_EXNS
  eargs[0] = scheme_make_immutable_sized_utf8_string(buffer, alen);
  eargs[1] = TMP_CMARK_VALUE;
  if (errno_val) {
    if (id == MZEXN_FAIL_FILESYSTEM) {
      id = MZEXN_FAIL_FILESYSTEM_ERRNO;
      eargs[2] = errno_val;
      c++;
    } else if (id == MZEXN_FAIL_NETWORK) {
      id = MZEXN_FAIL_NETWORK_ERRNO;
      eargs[2] = errno_val;
      c++;
    }
  }

  do_raise(scheme_make_struct_instance(exn_table[id].type,
				       c, eargs),
	   1,
           1);
#else
  call_error(buffer, alen, scheme_false);
#endif
}

#ifndef NO_SCHEME_EXNS

static Scheme_Object *
def_exn_handler(int argc, Scheme_Object *argv[])
{
  char *s;
  intptr_t len = -1;

  if (SCHEME_CHAPERONE_STRUCTP(argv[0])
      && scheme_is_struct_instance(exn_table[MZEXN].type, argv[0])) {
    Scheme_Object *str;
    str = scheme_struct_ref(argv[0], 0);
    if (SCHEME_CHAR_STRINGP(str)) {
      str = scheme_char_string_to_byte_string(str);
      s = SCHEME_BYTE_STR_VAL(str);
      len = SCHEME_BYTE_STRTAG_VAL(str);
    } else
      s = "exception raised [message field is not a string]";
  } else {
    char *v;

    v = scheme_make_provided_string(argv[0], 1, &len);
    s = scheme_malloc_atomic(len + 21);
    memcpy(s, "uncaught exception: ", 20);
    memcpy(s + 20, v, len + 1);
    len += 20;
  }

  call_error(s, len, argv[0]);

  return scheme_void;
}

static Scheme_Object *
init_exn_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("uncaught-exception-handler",
			     scheme_make_integer(MZCONFIG_INIT_EXN_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
nested_exn_handler(void *old_exn, int argc, Scheme_Object *argv[])
{
  Scheme_Object *arg = argv[0], *orig_arg = SCHEME_CDR((Scheme_Object *)old_exn);
  intptr_t len, mlen = -1, orig_mlen = -1, blen;
  char *buffer, *msg, *orig_msg, *raisetype, *orig_raisetype, *who, *sep;
  
  buffer = init_buf(&len, &blen);

  if (SCHEME_FALSEP(SCHEME_CAR((Scheme_Object *)old_exn))) {
    raisetype = "";
    sep = "";
    who = "handler for uncaught exceptions";
    msg = "did not escape";
  } else {
    who = SCHEME_BYTE_STR_VAL(SCHEME_CAR((Scheme_Object *)old_exn));
    sep = " by ";

    if (SCHEME_CHAPERONE_STRUCTP(arg)
        && scheme_is_struct_instance(exn_table[MZEXN].type, arg)) {
      Scheme_Object *str;
      str = scheme_struct_ref(arg, 0);
      raisetype = "exception raised";
      str = scheme_char_string_to_byte_string(str);
      msg = SCHEME_BYTE_STR_VAL(str);
      mlen = SCHEME_BYTE_STRLEN_VAL(str);
    } else {
      msg = error_write_to_string_w_max(arg, len, NULL);
      raisetype = "raise called (with non-exception value)";
    }
  }

  if (SCHEME_CHAPERONE_STRUCTP(orig_arg)
      && scheme_is_struct_instance(exn_table[MZEXN].type, orig_arg)) {
    Scheme_Object *str;
    str = scheme_struct_ref(orig_arg, 0);
    orig_raisetype = "exception raised";
    str = scheme_char_string_to_byte_string(str);
    orig_msg = SCHEME_BYTE_STR_VAL(str);
    orig_mlen = SCHEME_BYTE_STRLEN_VAL(str);
  } else {
    orig_msg = error_write_to_string_w_max(orig_arg, len, NULL);
    orig_raisetype = "raise called (with non-exception value)";
  }


  blen = scheme_sprintf(buffer, blen, "%s%s%s: %t; original %s: %t",
			raisetype, sep, who,
			msg, mlen,
			orig_raisetype,
			orig_msg, orig_mlen);
    
  call_error(buffer, blen, scheme_false);

  return scheme_void;
}

static void *do_raise_inside_barrier(void)
{
  Scheme_Object *arg;
  Scheme_Object *v, *p[1], *h, *marks;
  Scheme_Cont_Mark_Chain *chain;
  Scheme_Cont_Frame_Data cframe, cframe2;
  int got_chain;

  arg = scheme_current_thread->ku.k.p1;
  scheme_current_thread->ku.k.p1 = NULL;

  h = scheme_extract_one_cc_mark(NULL, scheme_exn_handler_key);

  chain = NULL;
  got_chain = 0;

  while (1) {
    if (!h) {
      h = scheme_get_param(scheme_current_config(), MZCONFIG_INIT_EXN_HANDLER);
      chain = NULL;
      got_chain = 1;
    }

    v = scheme_make_byte_string_without_copying("exception handler");
    v = scheme_make_closed_prim_w_arity(nested_exn_handler,
                                        scheme_make_pair(v, arg),
                                        "nested-exception-handler", 
                                        1, 1);

    scheme_push_continuation_frame(&cframe);
    scheme_set_cont_mark(scheme_exn_handler_key, v);
    scheme_push_break_enable(&cframe2, 0, 0);

    p[0] = arg;
    v = _scheme_apply(h, 1, p);

    scheme_pop_break_enable(&cframe2, 0);
    scheme_pop_continuation_frame(&cframe);

    /* Getting a value back means that we should chain to the
       next exception handler; we supply the returned value to
       the next exception handler (if any). */
    if (!got_chain) {
      marks = scheme_all_current_continuation_marks();
      chain = ((Scheme_Cont_Mark_Set *)marks)->chain;
      marks = NULL;
      /* Init chain to position of the handler we just
         called. */
      while (chain->key != scheme_exn_handler_key) {
        chain = chain->next;
      }
      got_chain = 1;
    }

    if (chain) {
      chain = chain->next;
      while (chain && (chain->key != scheme_exn_handler_key)) {
        chain = chain->next;
      }

      if (!chain)
        h = NULL; /* use uncaught handler */
      else
        h = chain->val;
      arg = v;
    } else {
      /* return from uncaught-exception handler */
      p[0] = scheme_false;
      return nested_exn_handler(scheme_make_pair(scheme_false, arg), 1, p);
    }
  }

  return scheme_void;
}

static Scheme_Object *
do_raise(Scheme_Object *arg, int need_debug, int eb)
{
  Scheme_Thread *p = scheme_current_thread;

  if (p->constant_folding) {
    if (p->constant_folding != (Optimize_Info *)scheme_false) {
      const char *msg;
      if (need_debug) {
        msg = scheme_display_to_string(((Scheme_Structure *)arg)->slots[0], NULL);
      } else
        msg = scheme_print_to_string(arg, NULL);
      scheme_log(scheme_optimize_info_logger(p->constant_folding),
                 SCHEME_LOG_WARNING,
                 0,
                 "warning%s: constant-fold attempt failed: %s",
                 scheme_optimize_info_context(p->constant_folding),
                 msg);
    }
    if (SCHEME_CHAPERONE_STRUCTP(arg)
        && scheme_is_struct_instance(exn_table[MZEXN_BREAK].type, arg)) {
      /* remember to re-raise exception */
      scheme_current_thread->reading_delayed = arg;
    }
    scheme_longjmp (scheme_error_buf, 1);
  }
  
  if (need_debug) {
    Scheme_Object *marks;
    marks = scheme_current_continuation_marks(NULL);
    ((Scheme_Structure *)arg)->slots[1] = marks;
  }

  p->ku.k.p1 = arg;

  if (eb)
    return (Scheme_Object *)scheme_top_level_do(do_raise_inside_barrier, 1);
  else
    return (Scheme_Object *)do_raise_inside_barrier();
}

static Scheme_Object *
sch_raise(int argc, Scheme_Object *argv[])
{
  if ((argc > 1) && SCHEME_FALSEP(argv[1]))
    return do_raise(argv[0], 0, 0);
  else
    return do_raise(argv[0], 0, 1);
}

void scheme_raise(Scheme_Object *exn)
{
  do_raise(exn, 0, 1);
}

typedef Scheme_Object (*Scheme_Struct_Field_Guard_Proc)(int argc, Scheme_Object *v);

static Scheme_Object *exn_field_check(int argc, Scheme_Object **argv)
{
  Scheme_Object *a[2], *v;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_field_contract(argv[2], "string?", argv[0]);
  if (!SAME_OBJ(argv[1], TMP_CMARK_VALUE) && !SCHEME_CONT_MARK_SETP(argv[1]))
    scheme_wrong_field_contract(argv[2], "continuation-mark-set?", argv[1]);

  a[0] = argv[0];
  a[1] = argv[1];
  
  if (!SCHEME_IMMUTABLE_CHAR_STRINGP(a[0])) {
    v = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(a[0]),
                                                SCHEME_CHAR_STRLEN_VAL(a[0]),
                                                1);
    a[0] = v;
  }

  return scheme_values(2, a);
}

static Scheme_Object *variable_field_check(int argc, Scheme_Object **argv)
{
  if (!SCHEME_SYMBOLP(argv[2]))
    scheme_wrong_field_contract(argv[3], "symbol?", argv[2]);

  return scheme_values(3, argv);
}

static Scheme_Object *syntax_field_check(int argc, Scheme_Object **argv)
{
  Scheme_Object *l;

  l = argv[2];
  while (SCHEME_PAIRP(l)) {
    if (!SCHEME_STXP(SCHEME_CAR(l)))
      break;
    l = SCHEME_CDR(l);
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_field_contract(argv[3], "(listof syntax?)", argv[2]);

  return scheme_values(3, argv);
}

static Scheme_Object *read_field_check(int argc, Scheme_Object **argv)
{
  Scheme_Object *l;

  l = argv[2];
  while (SCHEME_PAIRP(l)) {
    if (!scheme_is_location(SCHEME_CAR(l)))
      break;
    l = SCHEME_CDR(l);
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_field_contract(argv[3], "(listof srcloc?)", argv[2]);

  return scheme_values(3, argv);
}

static Scheme_Object *break_field_check(int argc, Scheme_Object **argv)
{
  if (!SCHEME_ECONTP(argv[2]))
    scheme_wrong_field_contract(argv[3], "escape-continuation?", argv[2]);

  return scheme_values(3, argv);
}

static Scheme_Object *errno_field_check(int argc, Scheme_Object **argv)
{
  if (!SCHEME_PAIRP(argv[2])
      || !scheme_exact_p(SCHEME_CAR(argv[2]))
      || !(SAME_OBJ(SCHEME_CDR(argv[2]), posix_symbol)
           || SAME_OBJ(SCHEME_CDR(argv[2]), windows_symbol)
           || SAME_OBJ(SCHEME_CDR(argv[2]), gai_symbol)))
    scheme_wrong_field_contract(argv[3], "(cons/c exact-integer? (or/c 'posix 'windows 'gai))", argv[2]);

  return scheme_values (3, argv);
}

static Scheme_Object *extract_syntax_locations(int argc, Scheme_Object **argv)
{
  if (scheme_is_struct_instance(exn_table[MZEXN_FAIL_SYNTAX].type, argv[0])) {
    Scheme_Object *stxs, *stx, *first = scheme_null, *last = NULL, *loco, *p;
    Scheme_Stx_Srcloc *loc;
    stxs = scheme_struct_ref(argv[0], 2);
    while (SCHEME_PAIRP(stxs)) {
      stx = SCHEME_CAR(stxs);
      loc = ((Scheme_Stx *)stx)->srcloc;
      loco = scheme_make_location(loc->src ? loc->src : scheme_false,
				  (loc->line >= 0) ? scheme_make_integer(loc->line) : scheme_false,
				  (loc->col >= 0) ? scheme_make_integer(loc->col-1) : scheme_false,
				  (loc->pos >= 0) ? scheme_make_integer(loc->pos) : scheme_false,
				  (loc->span >= 0) ? scheme_make_integer(loc->span) : scheme_false);
      p = scheme_make_pair(loco, scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      stxs = SCHEME_CDR(stxs);
    }
    return first;
  }
  scheme_wrong_contract("exn:fail:syntax-locations-accessor", "exn:fail:syntax?", 0, argc, argv);
  return NULL;
}

static Scheme_Object *extract_read_locations(int argc, Scheme_Object **argv)
{
  if (scheme_is_struct_instance(exn_table[MZEXN_FAIL_READ].type, argv[0]))
    return scheme_struct_ref(argv[0], 2);
  scheme_wrong_contract("exn:fail:read-locations-accessor", "exn:fail:read?", 0, argc, argv);
  return NULL;
}

void scheme_init_exn(Scheme_Env *env)
{
  int i, j;
  Scheme_Object *tmpo, **tmpop;

#define _MZEXN_DECL_FIELDS
# include "schexn.h"
#undef _MZEXN_DECL_FIELDS
#define _MZEXN_DECL_PROPS
# include "schexn.h"
#undef _MZEXN_DECL_PROPS

  REGISTER_SO(exn_table);

#ifdef MEMORY_COUNTING_ON
# ifndef GLOBAL_EXN_TABLE
  scheme_misc_count += (sizeof(exn_rec) * MZEXN_OTHER);
# endif
#endif

#define _MZEXN_PRESETUP
# include "schexn.h"
#undef _MZEXN_PRESETUP

#define EXN_PARENT(id) exn_table[id].type

#define EXN_FLAGS (SCHEME_STRUCT_EXPTIME | SCHEME_STRUCT_NO_SET | SCHEME_STRUCT_NO_MAKE_PREFIX)

#define SETUP_STRUCT(id, parent, name, argc, args, props, guard) \
    { tmpo = scheme_make_struct_type_from_string(name, parent, argc, props, guard, 1); \
      exn_table[id].type = tmpo; \
      tmpop = scheme_make_struct_names_from_array(name, argc, args, EXN_FLAGS, &exn_table[id].count); \
      exn_table[id].names = tmpop; }

#define EXNCONS scheme_make_pair
#define _MZEXN_SETUP
#include "schexn.h"

  for (i = 0; i < MZEXN_OTHER; i++) {
    if (exn_table[i].count) {
      Scheme_Object **values;

      scheme_force_struct_type_info((Scheme_Struct_Type *)exn_table[i].type);
      values = scheme_make_struct_values(exn_table[i].type,
					 exn_table[i].names,
					 exn_table[i].count,
					 EXN_FLAGS);
      for (j = exn_table[i].count - 1; j--; ) {
	scheme_add_global_constant_symbol(exn_table[i].names[j],
					  values[j],
					  env);
      }
    }
  }

  scheme_add_global_constant("uncaught-exception-handler",
			     scheme_register_parameter(init_exn_handler,
						       "uncaught-exception-handler",
						       MZCONFIG_INIT_EXN_HANDLER),
			     env);

  scheme_add_global_constant("raise",
			     scheme_make_noncm_prim(sch_raise,
                                                    "raise",
                                                    1, 2),
			     env);
}

void scheme_init_exn_config(void)
{
  Scheme_Object *h;

  h = scheme_make_prim_w_arity(def_exn_handler, "default-exception-handler", 1, 1);

  scheme_set_root_param(MZCONFIG_INIT_EXN_HANDLER, h);
}

#endif
