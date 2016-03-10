 /*
  Racket
  Copyright (c) 2004-2016 PLT Design Inc.
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

/* This file contains the Racket reader, including the normal reader
   and the one for .zo files. The normal reader is a recursive-descent
   parser. The really messy part is number parsing, which is in a
   different file, numstr.c. */

/* Rule on using scheme_ungetc(): the reader is generally allowed to
   use scheme_ungetc() only when it will definitely re-read the
   character as it continues. If the character will not be re-read
   (e.g., because an exception will be raised), then the reader must
   peek, instead. However, read-symbol uses ungetc() if the port does
   not have a specific peek handler, and in that case, read-symbol
   only ungetc()s a single character (that had been read by itself). */

#include "schpriv.h"
#include "schmach.h"
#include "schminc.h"
#include "schcpt.h"
#include "schvers.h"
#include <stdlib.h>
#include <ctype.h>
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif

#ifdef __clang__
# ifdef MZ_PRECISE_GC
#  pragma clang diagnostic ignored "-Wself-assign"
# endif
#endif

#define MAX_QUICK_SYMBOL_SIZE 64

/* Init options for embedding: */
/* these are used to set initial config parameterizations */
SHARED_OK int scheme_square_brackets_are_parens = 1;
SHARED_OK int scheme_curly_braces_are_parens = 1;
/* global flag set from environment variable */
SHARED_OK static int use_perma_cache = 1;

THREAD_LOCAL_DECL(int scheme_num_read_syntax_objects);


/* read-only global symbols */
SHARED_OK static char *builtin_fast;
SHARED_OK static unsigned char delim[128];
SHARED_OK static unsigned char cpt_branch[256];

/* Table of built-in variable refs for .zo loading: */
SHARED_OK static Scheme_Object **variable_references;
SHARED_OK int unsafe_variable_references_start;

ROSYM static Scheme_Object *quote_symbol;
ROSYM static Scheme_Object *quasiquote_symbol;
ROSYM static Scheme_Object *unquote_symbol;
ROSYM static Scheme_Object *unquote_splicing_symbol;
ROSYM static Scheme_Object *syntax_symbol;
ROSYM static Scheme_Object *unsyntax_symbol;
ROSYM static Scheme_Object *unsyntax_splicing_symbol;
ROSYM static Scheme_Object *quasisyntax_symbol;
ROSYM static Scheme_Object *brackets_symbol;
ROSYM static Scheme_Object *braces_symbol;
ROSYM static Scheme_Object *dot_symbol;
ROSYM static Scheme_Object *terminating_macro_symbol;
ROSYM static Scheme_Object *non_terminating_macro_symbol;
ROSYM static Scheme_Object *dispatch_macro_symbol;
/* For recoginizing unresolved hash tables and commented-out graph introductions: */
ROSYM static Scheme_Object *unresolved_uninterned_symbol;
ROSYM static Scheme_Object *tainted_uninterned_symbol;

/* local function prototypes */
static Scheme_Object *read_case_sensitive(int, Scheme_Object *[]);
static Scheme_Object *read_bracket_as_paren(int, Scheme_Object *[]);
static Scheme_Object *read_brace_as_paren(int, Scheme_Object *[]);
static Scheme_Object *read_bracket_with_tag(int, Scheme_Object *[]);
static Scheme_Object *read_brace_with_tag(int, Scheme_Object *[]);
static Scheme_Object *read_cdot(int, Scheme_Object *[]);
static Scheme_Object *read_accept_graph(int, Scheme_Object *[]);
static Scheme_Object *read_accept_compiled(int, Scheme_Object *[]);
static Scheme_Object *read_accept_box(int, Scheme_Object *[]);
static Scheme_Object *read_accept_pipe_quote(int, Scheme_Object *[]);
static Scheme_Object *read_decimal_as_inexact(int, Scheme_Object *[]);
static Scheme_Object *read_accept_dot(int, Scheme_Object *[]);
static Scheme_Object *read_accept_infix_dot(int, Scheme_Object *[]);
static Scheme_Object *read_accept_quasi(int, Scheme_Object *[]);
static Scheme_Object *read_accept_reader(int, Scheme_Object *[]);
static Scheme_Object *read_accept_lang(int, Scheme_Object *[]);
#ifdef LOAD_ON_DEMAND
static Scheme_Object *read_delay_load(int, Scheme_Object *[]);
#endif
static Scheme_Object *print_graph(int, Scheme_Object *[]);
static Scheme_Object *print_struct(int, Scheme_Object *[]);
static Scheme_Object *print_box(int, Scheme_Object *[]);
static Scheme_Object *print_vec_shorthand(int, Scheme_Object *[]);
static Scheme_Object *print_hash_table(int, Scheme_Object *[]);
static Scheme_Object *print_unreadable(int, Scheme_Object *[]);
static Scheme_Object *print_pair_curly(int, Scheme_Object *[]);
static Scheme_Object *print_mpair_curly(int, Scheme_Object *[]);
static Scheme_Object *print_syntax_width(int, Scheme_Object *[]);
static Scheme_Object *print_reader(int, Scheme_Object *[]);
static Scheme_Object *print_as_qq(int, Scheme_Object *[]);
static Scheme_Object *print_long_bool(int, Scheme_Object *[]);

#define NOT_EOF_OR_SPECIAL(x) ((x) >= 0)

#define mzSPAN(port, pos)  ()

#define NOT_ENABLED_str " not enabled in the current context"

#define isdigit_ascii(n) ((n >= '0') && (n <= '9'))

#define scheme_isxdigit(n) (isdigit_ascii(n) || ((n >= 'a') && (n <= 'f')) || ((n >= 'A') && (n <= 'F')))

#define RETURN_FOR_SPECIAL_COMMENT  0x1
#define RETURN_FOR_HASH_COMMENT     0x2
#define RETURN_FOR_DELIM            0x4
#define RETURN_FOR_COMMENT          0x8

static MZ_INLINE intptr_t SPAN(Scheme_Object *port, intptr_t pos) {
  intptr_t cpos;
  scheme_tell_all(port, NULL, NULL, &cpos);
  return cpos - pos + 1;
}

/* For cases where we'd rather report the location as just the relevant prefix: */
#define MINSPAN(port, pos, span) (span)

#define mz_shape_cons 0
#define mz_shape_vec 1
#define mz_shape_hash_list 2
#define mz_shape_hash_elem 3
#define mz_shape_vec_plus_infix 4
#define mz_shape_fl_vec 5
#define mz_shape_fx_vec 6

typedef struct Readtable {
  Scheme_Object so;
  Scheme_Hash_Table *mapping; /* pos int -> (cons int proc-or-char); neg int -> proc */
  char *fast_mapping;
  Scheme_Object *symbol_parser; /* NULL or a Racket function */
  char **names; /* error-message names */
} Readtable;

typedef struct ReadParams {
  MZTAG_IF_REQUIRED
  char can_read_compiled;
  char can_read_unsafe;
  char can_read_pipe_quote;
  char can_read_box;
  char can_read_graph;
  char can_read_reader;
  char can_read_lang;
  char case_sensitive;
  char square_brackets_are_parens;
  char curly_braces_are_parens;
  char square_brackets_are_tagged;
  char curly_braces_are_tagged;
  char read_cdot;
  char read_decimal_inexact;
  char can_read_dot;
  char can_read_infix_dot;
  char can_read_quasi;
  char skip_zo_vers_check;
  Readtable *table;
  Scheme_Object *magic_sym, *magic_val;
  Scheme_Object *delay_load_info;
  Scheme_Object *read_relative_path;
} ReadParams;

#define THREAD_FOR_LOCALS scheme_current_thread

static Scheme_Object *read_list(Scheme_Object *port, Scheme_Object *stxsrc,
				intptr_t line, intptr_t col, intptr_t pos,
				int opener, int closer,
				int shape, int use_stack,
				Scheme_Hash_Table **ht,
				Scheme_Object *indentation,
				ReadParams *params,
                                Readtable *table);
static Scheme_Object *read_string(int is_byte,
				  Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation,
				  ReadParams *params, Readtable *table,
                                  int err_ok);
static Scheme_Object *read_here_string(Scheme_Object *port, Scheme_Object *stxsrc,
				       intptr_t line, intptr_t col, intptr_t pos,
				       Scheme_Object *indentation,
				       ReadParams *params);
static Scheme_Object *read_quote(char *who, Scheme_Object *quote_symbol, int len,
				 Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				 Scheme_Hash_Table **ht,
				 Scheme_Object *indentation,
				 ReadParams *params);
static Scheme_Object *read_vector(Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				  int opener, char closer,
				  intptr_t reqLen, const mzchar *reqBuffer,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation,
				  ReadParams *params, Readtable *table,
                                  int allow_infix);
static Scheme_Object *read_flvector (Scheme_Object *port, Scheme_Object *stxsrc, 
                                     intptr_t line, intptr_t col, intptr_t pos,
                                     int opener, char closer,
                                     intptr_t requestLength, const mzchar *reqBuffer,
                                     Scheme_Hash_Table **ht,
                                     Scheme_Object *indentation,
                                     ReadParams *params, Readtable *table,
                                     int allow_infix);
static Scheme_Object *read_fxvector (Scheme_Object *port, Scheme_Object *stxsrc, 
                                     intptr_t line, intptr_t col, intptr_t pos,
                                     int opener, char closer,
                                     intptr_t requestLength, const mzchar *reqBuffer,
                                     Scheme_Hash_Table **ht,
                                     Scheme_Object *indentation,
                                     ReadParams *params, Readtable *table,
                                     int allow_infix);
static Scheme_Object *read_number(int init_ch,
				  Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				  int, int, int, int,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation,
				  ReadParams *params,
				  Readtable *table);
static Scheme_Object *read_symbol(int init_ch, int skip_rt,
				  Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation,
				  ReadParams *params,
				  Readtable *table);
static Scheme_Object *read_keyword(int init_ch,
				   Scheme_Object *port, Scheme_Object *stxsrc,
				   intptr_t line, intptr_t col, intptr_t pos,
				   Scheme_Hash_Table **ht,
				   Scheme_Object *indentation,
				   ReadParams *params,
				   Readtable *table);
static Scheme_Object  *read_delimited_constant(int ch, const mzchar *str,
                                               Scheme_Object *v,
                                               Scheme_Object *port,
                                               Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
                                               Scheme_Object *indentation,
                                               ReadParams *params, Readtable *table);
static Scheme_Object *read_character(Scheme_Object *port, Scheme_Object *stcsrc,
				     intptr_t line, intptr_t col, intptr_t pos,
				     Scheme_Hash_Table **ht,
				     Scheme_Object *indentation,
				     ReadParams *params);
static Scheme_Object *read_box(Scheme_Object *port, Scheme_Object *stxsrc,
			       intptr_t line, intptr_t col, intptr_t pos,
			       Scheme_Hash_Table **ht,
			       Scheme_Object *indentation,
			       ReadParams *params);
static Scheme_Object *read_hash(Scheme_Object *port, Scheme_Object *stxsrc,
				intptr_t line, intptr_t col, intptr_t pos,
				int opener, char closer, int kind,
				Scheme_Hash_Table **ht,
				Scheme_Object *indentation,
				ReadParams *params, Readtable *table);
static Scheme_Object *read_reader(Scheme_Object *port, Scheme_Object *stxsrc,
				  intptr_t line, intptr_t col, intptr_t pos,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation,
				  ReadParams *params);
static Scheme_Object *read_lang(Scheme_Object *port, Scheme_Object *stxsrc,
                                intptr_t line, intptr_t col, intptr_t pos,
                                int get_info,
                                Scheme_Hash_Table **ht,
                                Scheme_Object *indentation,
                                ReadParams *params,
                                int init_ch);
static Scheme_Object *read_compiled(Scheme_Object *port, Scheme_Object *stxsrc,
				    intptr_t line, intptr_t col, intptr_t pos,
				    Scheme_Hash_Table **ht,
				    ReadParams *params);
static void unexpected_closer(int ch,
			      Scheme_Object *port, Scheme_Object *stxsrc,
			      intptr_t line, intptr_t col, intptr_t pos,
			      Scheme_Object *indentation,
                              ReadParams *params);
static Scheme_Object *expected_lang(const char *prefix, int ch,
                                    Scheme_Object *port, Scheme_Object *stxsrc,
                                    intptr_t line, intptr_t col, intptr_t pos,
                                    int get_info);
static void pop_indentation(Scheme_Object *indentation);
static int next_is_delim(Scheme_Object *port,
			 ReadParams *params,
			 int brackets,
			 int braces);

static int skip_whitespace_comments(Scheme_Object *port, Scheme_Object *stxsrc,
				    Scheme_Hash_Table **ht,
				    Scheme_Object *indentation,
				    ReadParams *params, Readtable *table,
                                    Scheme_Object **prefetched);

static Scheme_Object *readtable_call(int w_char, int ch, Scheme_Object *proc, ReadParams *params,
				     Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
                                     int get_info,
				     Scheme_Hash_Table **ht, Scheme_Object *modpath_stx);
static Scheme_Object *read_flonum(Scheme_Object *port, 
				  Scheme_Object *stxsrc, 
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation, 
				  ReadParams *params,
				  int comment_mode);
static Scheme_Object *read_fixnum(Scheme_Object *port, 
				  Scheme_Object *stxsrc, 
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation, 
				  ReadParams *params,
				  int comment_mode);
static Scheme_Object *read_number_literal(Scheme_Object *port, 
                                          Scheme_Object *stxsrc, 
                                          int is_float, int is_not_float,
                                          Scheme_Hash_Table **ht,
                                          Scheme_Object *indentation, 
                                          ReadParams *params,
                                          int comment_mode);

#define READTABLE_WHITESPACE 0x1
#define READTABLE_CONTINUING 0x2
#define READTABLE_TERMINATING 0x4
#define READTABLE_SINGLE_ESCAPE 0x8
#define READTABLE_MULTIPLE_ESCAPE 0x10
#define READTABLE_MAPPED 0x20
static int readtable_kind(Readtable *t, int ch, ReadParams *params);
static Scheme_Object *readtable_handle(Readtable *t, int *_ch, int *_use_default, ReadParams *params,
				       Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
				       Scheme_Hash_Table **ht);
static Scheme_Object *readtable_handle_hash(Readtable *t, int ch, int *_use_default, ReadParams *params,
					    Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
					    Scheme_Hash_Table **ht);
static int readtable_effective_char(Readtable *t, int ch);
static Scheme_Object *make_readtable(int argc, Scheme_Object **argv);
static Scheme_Object *readtable_p(int argc, Scheme_Object **argv);
static Scheme_Object *readtable_mapping(int argc, Scheme_Object **argv);
static Scheme_Object *current_readtable(int argc, Scheme_Object **argv);
static Scheme_Object *current_reader_guard(int argc, Scheme_Object **argv);

static Scheme_Object *read_intern(int argc, Scheme_Object **argv);

/* A list stack is used to speed up the creation of intermediate lists
   during .zo reading. */

#define NUM_CELLS_PER_STACK 500

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef struct {
  Scheme_Type type;
  char closer;      /* expected close parent, bracket, etc. */
  char suspicious_closer; /* expected closer when suspicious line found */
  char multiline;   /* set to 1 if the match attempt spans a line */
  intptr_t start_line;  /* opener's line */
  intptr_t last_line;   /* current line, already checked the identation */
  intptr_t suspicious_line; /* non-0 => first suspicious line since opener */
  intptr_t max_indent;  /* max indentation encountered so far since opener,
		       not counting indentation brackets by a more neseted
		       opener */
  intptr_t suspicious_quote; /* non-0 => first suspicious quote whose closer
			    is on a different line */
} Scheme_Indent;

#define SCHEME_OK          0x1

#define is_lang_nonsep_char(ch) (scheme_isalpha(ch)     \
                                 || scheme_isdigit(ch)  \
                                 || ((ch) == '-')       \
                                 || ((ch) == '+')       \
                                 || ((ch) == '_'))

#define NEXT_LINE_CHAR 0x85
#define LINE_SEPARATOR_CHAR 0x2028
#define PARAGRAPH_SEPARATOR_CHAR 0x2029
#define is_line_comment_end(ch) ((ch == '\n') || (ch == '\r') \
                                 || (ch == NEXT_LINE_CHAR) \
                                 || (ch == LINE_SEPARATOR_CHAR) \
                                 || (ch == PARAGRAPH_SEPARATOR_CHAR))

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void scheme_init_read(Scheme_Env *env)
{
  REGISTER_SO(quote_symbol);
  REGISTER_SO(quasiquote_symbol);
  REGISTER_SO(unquote_symbol);
  REGISTER_SO(unquote_splicing_symbol);
  REGISTER_SO(syntax_symbol);
  REGISTER_SO(unsyntax_symbol);
  REGISTER_SO(unsyntax_splicing_symbol);
  REGISTER_SO(quasisyntax_symbol);

  REGISTER_SO(brackets_symbol);
  REGISTER_SO(braces_symbol);
  REGISTER_SO(dot_symbol);

  REGISTER_SO(unresolved_uninterned_symbol);
  REGISTER_SO(tainted_uninterned_symbol);
  REGISTER_SO(terminating_macro_symbol);
  REGISTER_SO(non_terminating_macro_symbol);
  REGISTER_SO(dispatch_macro_symbol);
  REGISTER_SO(builtin_fast);

  quote_symbol                  = scheme_intern_symbol("quote");
  quasiquote_symbol             = scheme_intern_symbol("quasiquote");
  unquote_symbol                = scheme_intern_symbol("unquote");
  unquote_splicing_symbol       = scheme_intern_symbol("unquote-splicing");
  syntax_symbol                 = scheme_intern_symbol("syntax");
  unsyntax_symbol               = scheme_intern_symbol("unsyntax");
  unsyntax_splicing_symbol      = scheme_intern_symbol("unsyntax-splicing");
  quasisyntax_symbol            = scheme_intern_symbol("quasisyntax");

  brackets_symbol               = scheme_intern_symbol("#%brackets");
  braces_symbol                 = scheme_intern_symbol("#%braces");
  dot_symbol                    = scheme_intern_symbol("#%dot");

  unresolved_uninterned_symbol = scheme_make_symbol("unresolved");
  tainted_uninterned_symbol    = scheme_make_symbol("tainted");
    
  terminating_macro_symbol     = scheme_intern_symbol("terminating-macro");
  non_terminating_macro_symbol = scheme_intern_symbol("non-terminating-macro");
  dispatch_macro_symbol        = scheme_intern_symbol("dispatch-macro");

  /* initialize builtin_fast */
  {
    int i; 
    builtin_fast = scheme_malloc_atomic(128);
    memset(builtin_fast, READTABLE_CONTINUING, 128);
    for (i = 0; i < 128; i++) {
      if (scheme_isspace(i))
        builtin_fast[i] = READTABLE_WHITESPACE;
    }
    builtin_fast[';']  = READTABLE_TERMINATING;
    builtin_fast['\''] = READTABLE_TERMINATING;
    builtin_fast['`']  = READTABLE_TERMINATING;
    builtin_fast[',']  = READTABLE_TERMINATING;
    builtin_fast['"']  = READTABLE_TERMINATING;
    builtin_fast['|']  = READTABLE_MULTIPLE_ESCAPE;
    builtin_fast['\\'] = READTABLE_SINGLE_ESCAPE;
    builtin_fast['(']  = READTABLE_TERMINATING;
    builtin_fast['[']  = READTABLE_TERMINATING;
    builtin_fast['{']  = READTABLE_TERMINATING;
    builtin_fast[')']  = READTABLE_TERMINATING;
    builtin_fast[']']  = READTABLE_TERMINATING;
    builtin_fast['}']  = READTABLE_TERMINATING;
  }

  /* initialize cpt_branch */
  {
    int i;

    for (i = 0; i < 256; i++) {
      cpt_branch[i] = i;
    }

#define FILL_IN(v) \
    for (i = CPT_ ## v ## _START; i < CPT_ ## v ## _END; i++) { \
      cpt_branch[i] = CPT_ ## v ## _START; \
    }
    FILL_IN(SMALL_NUMBER);
    FILL_IN(SMALL_SYMBOL);
    FILL_IN(SMALL_MARSHALLED);
    FILL_IN(SMALL_LIST);
    FILL_IN(SMALL_PROPER_LIST);
    FILL_IN(SMALL_LOCAL);
    FILL_IN(SMALL_LOCAL_UNBOX);
    FILL_IN(SMALL_SVECTOR);
    FILL_IN(SMALL_APPLICATION);

    /* These two are handled specially: */
    cpt_branch[CPT_SMALL_APPLICATION2] = CPT_SMALL_APPLICATION2;
    cpt_branch[CPT_SMALL_APPLICATION3] = CPT_SMALL_APPLICATION3;
  }
  
  {
    int i;
    for (i = 0; i < 128; i++) {
      delim[i] = SCHEME_OK;
    }
    delim['('] -= SCHEME_OK;
    delim[')'] -= SCHEME_OK;
    delim['['] -= SCHEME_OK;
    delim[']'] -= SCHEME_OK;
    delim['{'] -= SCHEME_OK;
    delim['}'] -= SCHEME_OK;
    delim['"'] -= SCHEME_OK;
    delim['\''] -= SCHEME_OK;
    delim[','] -= SCHEME_OK;
    delim[';'] -= SCHEME_OK;
    delim['`'] -= SCHEME_OK;
  }

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  GLOBAL_PARAMETER("current-readtable",             current_readtable,      MZCONFIG_READTABLE,                   env);
  GLOBAL_PARAMETER("current-reader-guard",          current_reader_guard,   MZCONFIG_READER_GUARD,                env);
  GLOBAL_PARAMETER("read-case-sensitive",           read_case_sensitive,    MZCONFIG_CASE_SENS,                   env);
  GLOBAL_PARAMETER("read-square-bracket-as-paren",  read_bracket_as_paren,  MZCONFIG_SQUARE_BRACKETS_ARE_PARENS,  env);
  GLOBAL_PARAMETER("read-curly-brace-as-paren",     read_brace_as_paren,    MZCONFIG_CURLY_BRACES_ARE_PARENS,     env);
  GLOBAL_PARAMETER("read-square-bracket-with-tag",  read_bracket_with_tag,  MZCONFIG_SQUARE_BRACKETS_ARE_TAGGED,  env);
  GLOBAL_PARAMETER("read-curly-brace-with-tag",     read_brace_with_tag,    MZCONFIG_CURLY_BRACES_ARE_TAGGED,     env);
  GLOBAL_PARAMETER("read-cdot",                     read_cdot,              MZCONFIG_READ_CDOT,                   env);
  GLOBAL_PARAMETER("read-accept-graph",             read_accept_graph,      MZCONFIG_CAN_READ_GRAPH,              env);
  GLOBAL_PARAMETER("read-accept-compiled",          read_accept_compiled,   MZCONFIG_CAN_READ_COMPILED,           env);
  GLOBAL_PARAMETER("read-accept-box",               read_accept_box,        MZCONFIG_CAN_READ_BOX,                env);
  GLOBAL_PARAMETER("read-accept-bar-quote",         read_accept_pipe_quote, MZCONFIG_CAN_READ_PIPE_QUOTE,         env);
  GLOBAL_PARAMETER("read-decimal-as-inexact",       read_decimal_as_inexact,MZCONFIG_READ_DECIMAL_INEXACT,        env);
  GLOBAL_PARAMETER("read-accept-dot",               read_accept_dot,        MZCONFIG_CAN_READ_DOT,                env);
  GLOBAL_PARAMETER("read-accept-infix-dot",         read_accept_infix_dot,  MZCONFIG_CAN_READ_INFIX_DOT,          env);
  GLOBAL_PARAMETER("read-accept-quasiquote",        read_accept_quasi,      MZCONFIG_CAN_READ_QUASI,              env);
  GLOBAL_PARAMETER("read-accept-reader",            read_accept_reader,     MZCONFIG_CAN_READ_READER,             env);
  GLOBAL_PARAMETER("read-accept-lang",              read_accept_lang,       MZCONFIG_CAN_READ_LANG,               env);
#ifdef LOAD_ON_DEMAND
  GLOBAL_PARAMETER("read-on-demand-source",         read_delay_load,        MZCONFIG_DELAY_LOAD_INFO,             env);
#endif
  GLOBAL_PARAMETER("print-graph",                   print_graph,            MZCONFIG_PRINT_GRAPH,                 env);
  GLOBAL_PARAMETER("print-struct",                  print_struct,           MZCONFIG_PRINT_STRUCT,                env);
  GLOBAL_PARAMETER("print-box",                     print_box,              MZCONFIG_PRINT_BOX,                   env);
  GLOBAL_PARAMETER("print-vector-length",           print_vec_shorthand,    MZCONFIG_PRINT_VEC_SHORTHAND,         env);
  GLOBAL_PARAMETER("print-hash-table",              print_hash_table,       MZCONFIG_PRINT_HASH_TABLE,            env);
  GLOBAL_PARAMETER("print-unreadable",              print_unreadable,       MZCONFIG_PRINT_UNREADABLE,            env);
  GLOBAL_PARAMETER("print-pair-curly-braces",       print_pair_curly,       MZCONFIG_PRINT_PAIR_CURLY,            env);
  GLOBAL_PARAMETER("print-mpair-curly-braces",      print_mpair_curly,      MZCONFIG_PRINT_MPAIR_CURLY,           env);
  GLOBAL_PARAMETER("print-syntax-width",            print_syntax_width,     MZCONFIG_PRINT_SYNTAX_WIDTH,          env);
  GLOBAL_PARAMETER("print-reader-abbreviations",    print_reader,           MZCONFIG_PRINT_READER,                env);
  GLOBAL_PARAMETER("print-boolean-long-form",       print_long_bool,        MZCONFIG_PRINT_LONG_BOOLEAN,          env);
  GLOBAL_PARAMETER("print-as-expression",           print_as_qq,            MZCONFIG_PRINT_AS_QQ,                 env);

  GLOBAL_PRIM_W_ARITY("make-readtable",     make_readtable,     1, -1,      env);
  GLOBAL_FOLDING_PRIM("readtable?",         readtable_p,        1, 1, 1,    env);
  GLOBAL_PRIM_W_ARITY2("readtable-mapping", readtable_mapping,  2, 2, 3, 3, env);

  GLOBAL_NONCM_PRIM("datum-intern-literal", read_intern, 1, 1, env);

  if (getenv("PLT_DELAY_FROM_ZO")) {
    use_perma_cache = 0;
  }
}

void scheme_init_variable_references_constants()
{
  REGISTER_SO(variable_references);
  variable_references = scheme_make_builtin_references_table(&unsafe_variable_references_start);
}


static void track_indentation(Scheme_Object *indentation, int line, int col)
{
  if (!SCHEME_NULLP(indentation)) {
    Scheme_Indent *indt = (Scheme_Indent *)SCHEME_CAR(indentation);
    /* Already checked this line? */
    if (line > indt->last_line) {
      indt->last_line = line;
      indt->multiline = 1;
      /* At least as indented as before? */
      if (col >= indt->max_indent)
	indt->max_indent = col;
      else if (!indt->suspicious_line) {
	/* Not as indented, and no suspicious line found
	   already. Suspect that the closer should have
	   appeared earlier. */
	indt->suspicious_closer = indt->closer;
	indt->suspicious_line = line;
      }
    }
  }
}

/*========================================================================*/
/*                             parameters                                 */
/*========================================================================*/

#define DO_CHAR_PARAM(name, pos) \
  return scheme_param_config(name, scheme_make_integer(pos), argc, argv, -1, NULL, NULL, 1)

static Scheme_Object *
read_case_sensitive(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-case-sensitive", MZCONFIG_CASE_SENS);
}

static Scheme_Object *
read_bracket_as_paren(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-square-bracket-as-paren", MZCONFIG_SQUARE_BRACKETS_ARE_PARENS);
}

static Scheme_Object *
read_brace_as_paren(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-curly-brace-as-paren", MZCONFIG_CURLY_BRACES_ARE_PARENS);
}

static Scheme_Object *
read_bracket_with_tag(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-square-bracket-with-tag", MZCONFIG_SQUARE_BRACKETS_ARE_TAGGED);
}

static Scheme_Object *
read_brace_with_tag(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-curly-brace-with-tag", MZCONFIG_CURLY_BRACES_ARE_TAGGED);
}

static Scheme_Object *
read_cdot(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-cdot", MZCONFIG_READ_CDOT);
}

static Scheme_Object *
read_accept_graph(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-graph", MZCONFIG_CAN_READ_GRAPH);
}

static Scheme_Object *
read_accept_compiled(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-compiled", MZCONFIG_CAN_READ_COMPILED);
}

static Scheme_Object *
read_accept_box(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-box", MZCONFIG_CAN_READ_BOX);
}

static Scheme_Object *
read_accept_pipe_quote(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-pipe-quote", MZCONFIG_CAN_READ_PIPE_QUOTE);
}

static Scheme_Object *
read_decimal_as_inexact(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-decimal-as-inexact", MZCONFIG_READ_DECIMAL_INEXACT);
}

static Scheme_Object *
read_accept_dot(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-dot", MZCONFIG_CAN_READ_DOT);
}

static Scheme_Object *
read_accept_infix_dot(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-infix-dot", MZCONFIG_CAN_READ_INFIX_DOT);
}

static Scheme_Object *
read_accept_quasi(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-quasiquote", MZCONFIG_CAN_READ_QUASI);
}

static Scheme_Object *
read_accept_reader(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-reader", MZCONFIG_CAN_READ_READER);
}

static Scheme_Object *
read_accept_lang(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-lang", MZCONFIG_CAN_READ_LANG);
}

static Scheme_Object *
print_graph(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-graph", MZCONFIG_PRINT_GRAPH);
}

static Scheme_Object *
print_struct(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-struct", MZCONFIG_PRINT_STRUCT);
}

static Scheme_Object *
print_box(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-box", MZCONFIG_PRINT_BOX);
}

static Scheme_Object *
print_vec_shorthand(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-vector-length", MZCONFIG_PRINT_VEC_SHORTHAND);
}

static Scheme_Object *
print_hash_table(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-hash-table", MZCONFIG_PRINT_HASH_TABLE);
}

static Scheme_Object *
print_unreadable(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-unreadable", MZCONFIG_PRINT_UNREADABLE);
}

static Scheme_Object *
print_pair_curly(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-pair-curly", MZCONFIG_PRINT_PAIR_CURLY);
}

static Scheme_Object *
print_mpair_curly(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-mpair-curly", MZCONFIG_PRINT_MPAIR_CURLY);
}

static Scheme_Object *
print_reader(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-reader-abbreviations", MZCONFIG_PRINT_READER);
}

static Scheme_Object *
print_as_qq(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-as-expression", MZCONFIG_PRINT_AS_QQ);
}

static Scheme_Object *
print_long_bool(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("print-boolean-long-form", MZCONFIG_PRINT_LONG_BOOLEAN);
}

static Scheme_Object *good_syntax_width(int c, Scheme_Object **argv)
{
  int ok;

  ok = (SCHEME_INTP(argv[0]) 
	? ((SCHEME_INT_VAL(argv[0]) > 3)
           || !SCHEME_INT_VAL(argv[0]))
	: (SCHEME_BIGNUMP(argv[0])
	   ? SCHEME_BIGPOS(argv[0])
	   : (SCHEME_DBLP(argv[0])
              ? MZ_IS_POS_INFINITY(SCHEME_DBL_VAL(argv[0]))
              : 0)));

  return ok ? scheme_true : scheme_false;
}

static Scheme_Object *
print_syntax_width(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("print-syntax-width",
                              scheme_make_integer(MZCONFIG_PRINT_SYNTAX_WIDTH),
                              argc, argv,
                              -1, good_syntax_width, 
                              "(or/c +inf.0 0 (and/c exact-integer? (>=/c 3)))", 0);
}

#ifdef LOAD_ON_DEMAND
static Scheme_Object *rdl_check(int argc, Scheme_Object **argv)
{
  Scheme_Object *s = argv[0];

  return ((SCHEME_FALSEP(s)
           || SAME_OBJ(scheme_true, s)
           || (SCHEME_PATHP(s)
               && scheme_is_complete_path(SCHEME_PATH_VAL(s),
                                          SCHEME_PATH_LEN(s),
                                          SCHEME_PLATFORM_PATH_KIND)))
          ? scheme_true : scheme_false);
}

static Scheme_Object *
read_delay_load(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("read-on-demand-source",
                              scheme_make_integer(MZCONFIG_DELAY_LOAD_INFO),
                              argc, argv,
                              -1, rdl_check, 
                              "(or/c #f #t (and/c path? complete-path?))",
                              0);

}
#endif

/*========================================================================*/
/*                             main read loop                             */
/*========================================================================*/

#ifdef DO_STACK_CHECK

static Scheme_Object *read_inner_inner_inner(Scheme_Object *port,
				       Scheme_Object *stxsrc,
				       Scheme_Hash_Table **ht,
				       Scheme_Object *indentation,
				       ReadParams *params,
				       int comment_mode,
				       int pre_char,
				       Readtable *init_readtable,
                                       int get_info);
static Scheme_Object *read_inner_inner(Scheme_Object *port,
				       Scheme_Object *stxsrc,
				       Scheme_Hash_Table **ht,
				       Scheme_Object *indentation,
				       ReadParams *params,
				       int comment_mode,
				       int pre_char,
				       Readtable *init_readtable,
                                       int get_info);
static Scheme_Object *read_inner(Scheme_Object *port, 
				 Scheme_Object *stxsrc, 
				 Scheme_Hash_Table **ht,
				 Scheme_Object *indentation, 
				 ReadParams *params,
				 int comment_mode);

static void set_need_copy(Scheme_Hash_Table **ht)
{
  /* Set indicator in *ht that we need to copy: */
  if (!*ht) {
    Scheme_Hash_Table *tht;
    tht = scheme_make_hash_table(SCHEME_hash_ptr);
    *ht = tht;
  }
  scheme_hash_set(*ht, tainted_uninterned_symbol, scheme_true);
}

static Scheme_Object *read_inner_inner_inner_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p2;
  Scheme_Object *stxsrc = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *indentation = SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  ReadParams *params = (ReadParams *)SCHEME_CDR((Scheme_Object *)p->ku.k.p4);
  Readtable *table = (Readtable *)p->ku.k.p5;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  return read_inner_inner_inner(o, stxsrc, ht, indentation, params, p->ku.k.i1, p->ku.k.i2,
                          table, p->ku.k.i3);
}
#endif

#define MAX_GRAPH_ID_DIGITS 8

static int read_vector_length(Scheme_Object *port, Readtable *table, int *ch, mzchar *tagbuf, mzchar *vecbuf, int *vector_length, int *digits, int *overflow)
{
  int i = 0, j = 0, nch;
  *vector_length = -1;
  *overflow = 0;
  *digits = 0;

  while (NOT_EOF_OR_SPECIAL((*ch)) && isdigit_ascii((*ch))) {
    if (*digits <= MAX_GRAPH_ID_DIGITS)
      (*digits)++;

    /* For vector error msgs, want to drop leading zeros: */
    if (j || ((*ch) != '0')) {
      if (j < 60) {
        vecbuf[j++] = (*ch);
      } else if (j == 60) {
        vecbuf[j++] = '.';
        vecbuf[j++] = '.';
        vecbuf[j++] = '.';
        vecbuf[j] = 0;
      }
    }

    /* For tag error msgs, want to keep zeros: */
    if (i < 60) {
      tagbuf[i++] = (*ch);
    } else if (i == 60) {
      tagbuf[i++] = '.';
      tagbuf[i++] = '.';
      tagbuf[i++] = '.';
      tagbuf[i] = 0;
    }

    if (!(*overflow)) {
      uintptr_t old_len;
      uintptr_t new_len;

      if (*vector_length < 0)
        *vector_length = 0;

      old_len = *vector_length;
      new_len = *vector_length;
      new_len = ((new_len) * 10) + ((*ch) - 48);
      *vector_length = new_len;
      if ((*vector_length < 0) || ((new_len / 10) != old_len)) {
        *overflow = 1;
      }
    }
    nch = scheme_getc_special_ok(port);
    (*ch) = nch;
  }

  if (*overflow)
    *vector_length = -2;
  vecbuf[j] = 0;
  tagbuf[i] = 0;

  if (!j) {
    vecbuf[j] = '0';
    vecbuf[0] = 0;
  }

  return readtable_effective_char(table, (*ch));
}

static Scheme_Object *
read_plus_minus_period_leading_number(Scheme_Object *port, Scheme_Object *stxsrc,
                                      int ch, intptr_t line, intptr_t col, intptr_t pos,
                                      int is_float, int is_not_float,
                                      Scheme_Hash_Table **ht, Scheme_Object *indentation, ReadParams *params,
                                      Readtable *table)
{
  int ch2;
  Scheme_Object *special_value;
  ch2 = scheme_peekc_special_ok(port);
  if ((NOT_EOF_OR_SPECIAL(ch2) && isdigit_ascii(ch2)) || (ch2 == '.')
      || ((ch2 == 'i') || (ch2 == 'I') /* Maybe inf */
          || (ch2 == 'n') || (ch2 == 'N') /* Maybe nan*/ )) {
    /* read_number tries to get a number, but produces a symbol if number parsing doesn't work,
       unless `is_float' or `is_not_float': */
    special_value = read_number(ch, port, stxsrc, line, col, pos, 
                                is_float, is_not_float, 10, 0, ht, indentation, params, table);
  } else {
    special_value = read_symbol(ch, 0, port, stxsrc, line, col, pos, ht, indentation, params, table);
  }
  return special_value;
}


static Scheme_Object *
read_inner_inner_inner(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht,
		 Scheme_Object *indentation, ReadParams *params,
		 int comment_mode, int pre_char, Readtable *table,
                 int get_info)
{
  int ch, ch2, depth, dispatch_ch, special_value_need_copy = 0;
  intptr_t line = 0, col = 0, pos = 0;
  Scheme_Object *special_value;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *pr;
      ReadParams *params2;

      /* params may be on the stack, so move it to the heap: */
      params2 = MALLOC_ONE_RT(ReadParams);
      memcpy(params2, params, sizeof(ReadParams));
#ifdef MZ_PRECISE_GC
      params2->type = scheme_rt_read_params;
#endif

      p->ku.k.p1 = (void *)port;
      p->ku.k.p2 = (void *)ht;
      p->ku.k.p3 = (void *)stxsrc;

      pr = scheme_make_pair(indentation, (Scheme_Object *)params2);
      p->ku.k.p4 = (void *)pr;

      p->ku.k.p5 = (void *)table;

      p->ku.k.i1 = comment_mode;
      p->ku.k.i2 = pre_char;
      p->ku.k.i3 = get_info;
      return scheme_handle_stack_overflow(read_inner_inner_inner_k);
    }
  }
#endif

 start_over:

  SCHEME_USE_FUEL(1);

  while (1) {
    if (pre_char >= 0) {
      ch = pre_char;
      pre_char = -1;
    } else
      ch = scheme_getc_special_ok(port);
    if (NOT_EOF_OR_SPECIAL(ch)) {
      if (table) {
	if (!(readtable_kind(table, ch, params) & READTABLE_WHITESPACE))
	  break;
      } else if (!scheme_isspace(ch))
	break;
    } else
      break;
  }

  scheme_tell_all(port, &line, &col, &pos);

  /* Found non-whitespace. Track indentation: */
  if (col >= 0) {
    if (SCHEME_PAIRP(indentation)) {
      int effective_ch;
      effective_ch = readtable_effective_char(table, ch);
      /* Ignore if it's a comment start or spurious closer: */
      if ((effective_ch != ';')
	  && !((effective_ch == '#') && (scheme_peekc_special_ok(port) == '|'))
	  && (effective_ch != ')')
	  && ((effective_ch != '}') || !params->curly_braces_are_parens)
	  && ((effective_ch != ']') || !params->square_brackets_are_parens)) {
	track_indentation(indentation, line, col);
      }
    }
  }

  special_value = NULL;
  if (table && NOT_EOF_OR_SPECIAL(ch)) {
    Scheme_Object *v;
    int use_default, ch2 = ch;
    v = readtable_handle(table, &ch2, &use_default, params,
			 port, stxsrc, line, col, pos, ht);
    if (!use_default) {
      dispatch_ch = SCHEME_SPECIAL;
      special_value = v;
    } else
      dispatch_ch = ch2;
  } else
    dispatch_ch = ch;

  if (get_info && (dispatch_ch != '#') && (dispatch_ch != ';')) {
    /* If ch is EOF, then col or pos wasn't incremented by reading ch.
       The col and pos might be used in an error message, which expects
       to subtract one from each --- so counteract by adding one here. */
    if (ch == EOF) {
      if (pos >= 0) pos++;
      if (col >= 0) col++;
    }
    return expected_lang("", ch, port, stxsrc, line, col, pos, get_info);
  }

  switch ( dispatch_ch )
    {
    case EOF: 
      return scheme_eof;
    case SCHEME_SPECIAL:
      {
	if (!special_value) {
	  special_value = scheme_get_special(port, stxsrc, line, col, pos, 0, ht);
	  special_value_need_copy = 1;
	}
	break;
      }
    case ']':
      if (!params->square_brackets_are_parens) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of close square bracket");
	return NULL;
      } else {
	unexpected_closer(ch, port, stxsrc, line, col, pos, indentation, params);
	return NULL;
      }
    case '}':
      if (!params->curly_braces_are_parens) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of close curly brace");
	return NULL;
      } else {
	unexpected_closer(ch, port, stxsrc, line, col, pos, indentation, params);
	return NULL;
      }
    case ')':
      unexpected_closer(ch, port, stxsrc, line, col, pos, indentation, params);
      return NULL;
    case '(':
      return read_list(port, stxsrc, line, col, pos, ch, ')', mz_shape_cons, 0, ht, indentation, params, table);
    case '[':
      if (!params->square_brackets_are_parens && !params->square_brackets_are_tagged) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of open square bracket");
	return NULL;
      } else
	return read_list(port, stxsrc, line, col, pos, ch, ']', mz_shape_cons, 0, ht, indentation, params, table);
    case '{':
      if (!params->curly_braces_are_parens && !params->curly_braces_are_tagged) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of open curly brace");
	return NULL;
      } else
	return read_list(port, stxsrc, line, col, pos, ch, '}', mz_shape_cons, 0, ht, indentation, params, table);
    case '|':
      special_value = read_symbol(ch, 1, port, stxsrc, line, col, pos, ht, indentation, params, table);
      break;
    case '"':
      return read_string(0, port, stxsrc, line, col, pos, ht, indentation, params, table, 1);
    case '\'':
      return read_quote("quoting '", quote_symbol, 1, port, stxsrc, line, col, pos, ht, indentation, params);
    case '`':
      if (!params->can_read_quasi) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of backquote");
	return NULL;
      } else
	return read_quote("quasiquoting `", quasiquote_symbol, 1, port, stxsrc, line, col, pos, ht, indentation, params);
    case ',':
      if (!params->can_read_quasi) {
	scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: illegal use of comma");
	return NULL;
      } else {
	if (scheme_peekc_special_ok(port) == '@') {
	  ch = scheme_getc(port); /* must be '@' */
	  return read_quote("unquoting ,@", unquote_splicing_symbol, 2, port, stxsrc, line, col, pos, ht, indentation, params);
	} else
	  return read_quote("unquoting ,", unquote_symbol, 1, port, stxsrc, line, col, pos, ht, indentation, params);
      }
    case ';':
      {
	while (((ch = scheme_getc_special_ok(port)) != '\n') 
               && !is_line_comment_end(ch)) {
	  if (ch == EOF) {
            if (comment_mode & RETURN_FOR_COMMENT)
              return NULL;
            if (get_info)
              return expected_lang("", ch, port, stxsrc, line, col, pos, get_info);
	    return scheme_eof;
          }
	  if (ch == SCHEME_SPECIAL)
	    scheme_get_ready_read_special(port, stxsrc, ht);
	}
	if ((table && (comment_mode & RETURN_FOR_SPECIAL_COMMENT))
	    || (comment_mode & RETURN_FOR_COMMENT))
	  return NULL;
	goto start_over;
      }
    case '+':
    case '-':
    case '.': /* ^^^ fallthrough ^^^ */
      special_value = read_plus_minus_period_leading_number(port, stxsrc, ch, line, col, pos, 0, 0, ht, indentation, params, table);
      break;
    case '#':
      ch = scheme_getc_special_ok(port);

      if (get_info && (ch != '|') && (ch != '!') && (ch != 'l') && (ch != ';')) {
        return expected_lang("#", ch, port, stxsrc, line, col, pos, get_info);
      }

      if (table) {
	Scheme_Object *v;
	int use_default;
	v = readtable_handle_hash(table, ch, &use_default, params,
				  port, stxsrc, line, col, pos, ht);
	if (!use_default) {
	  if (v)
	    return v;
	  if (comment_mode & RETURN_FOR_SPECIAL_COMMENT)
	    return NULL;
	  goto start_over;
	}
      }

      special_value = NULL;

      switch (ch)
	{
	case EOF:
	case SCHEME_SPECIAL:
	  scheme_read_err(port, stxsrc, line, col, pos, 1, ch, indentation, "read: bad syntax `#'");
	  break;
	case ';':
	  {
	    Scheme_Object *skipped;
	    skipped = read_inner(port, stxsrc, ht, indentation, params, 0);
	    if (SCHEME_EOFP(skipped))
	      scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, 2), EOF, indentation,
			      "read: expected a commented-out element for `#;' (found end-of-file)");
	    /* For resolving graphs introduced in #; : */
	    if (*ht) {
	      Scheme_Object *v;
	      v = scheme_hash_get(*ht, unresolved_uninterned_symbol);
	      if (!v)
		v = scheme_null;
	      v = scheme_make_pair(skipped, v);
	      scheme_hash_set(*ht, unresolved_uninterned_symbol, v);
	    }

	    if ((comment_mode & RETURN_FOR_HASH_COMMENT)
		|| (table && (comment_mode & RETURN_FOR_SPECIAL_COMMENT))
		|| (comment_mode & RETURN_FOR_COMMENT))
	      return NULL;

	    goto start_over;
	  }
	  break;
	case '%':
          scheme_ungetc('%', port);
          special_value = read_symbol('#', 1, port, stxsrc, line, col, pos, ht, indentation, params, table);
	  break;
	case ':':
          return read_keyword(-1, port, stxsrc, line, col, pos, ht, indentation, params, table);
	  break;
	case '(':
          return read_vector(port, stxsrc, line, col, pos, ch, ')', -1, NULL, ht, indentation, params, table, 0);
	  break;
	case '[':
          if (!params->square_brackets_are_parens) {
            scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation, "read: bad syntax `#['");
            return NULL;
          } else
            return read_vector(port, stxsrc, line, col, pos, ch, ']', -1, NULL, ht, indentation, params, table, 0);
	  break;
	case '{':
          if (!params->curly_braces_are_parens) {
            scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation, "read: bad syntax `#{'");
            return NULL;
          } else
            return read_vector(port, stxsrc, line, col, pos, ch, '}', -1, NULL, ht, indentation, params, table, 0);
	case '\\':
	  {
	    Scheme_Object *chr;
	    chr = read_character(port, stxsrc, line, col, pos, ht, indentation, params);
	    if (stxsrc)
	      chr = scheme_make_stx_w_offset(chr, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
	    return chr;
	  }
	  break;
	case 'T':
	case 't': 
          if (next_is_delim(port, params, 1, 1)) {
            /* found delimited `#t' */
            return (stxsrc
                    ? scheme_make_stx_w_offset(scheme_true, line, col, pos, 2, stxsrc, STX_SRCTAG)
                    : scheme_true);
          } else {
            GC_CAN_IGNORE const mzchar str[] = { 't', 'r', 'u', 'e', 0 };
            return read_delimited_constant(ch, str, scheme_true, port, stxsrc, line, col, pos, 
                                           indentation, params, table);
          }
	case 'F':
	case 'f': 
          if (next_is_delim(port, params, 1, 1)) {
            /* found delimited `#f' */
            return (stxsrc
                    ? scheme_make_stx_w_offset(scheme_false, line, col, pos, 2, stxsrc, STX_SRCTAG)
                    : scheme_false);
          } else {
            int next;
            next = scheme_peekc_special_ok(port);
            switch (next) {
              case 'l':
              case 'x': 
                {
                  int vector_length = -1;
                  int overflow = 0, digits = 0, effective_ch;
                  mzchar tagbuf[64], vecbuf[64]; /* just for errors */
                  int ch;

                  if (stxsrc) {
                    scheme_read_err(port, stxsrc, line, col, pos, 3, 0, indentation,
                                    "read-syntax: literal f%cvectors not allowed", next);
                    return NULL;
                  }

                  ch = scheme_getc_special_ok(port);
                  ch = scheme_getc_special_ok(port);
                  if (isdigit_ascii(ch))
                    effective_ch = read_vector_length(port, table, &ch, tagbuf, vecbuf, &vector_length, &digits, &overflow);
                  else 
                    effective_ch = ch; 
                  switch (effective_ch) {
                  case '(':
                    if (next == 'l')
                      return read_flvector(port, stxsrc, line, col, pos, ch, ')', vector_length, vecbuf, ht, indentation, params, table, 0);
                    else
                      return read_fxvector(port, stxsrc, line, col, pos, ch, ')', vector_length, vecbuf, ht, indentation, params, table, 0);
                    break;
                  case '[':
                    if (!params->square_brackets_are_parens) {
                      scheme_read_err(port, stxsrc, line, col, pos, 2, effective_ch, indentation, "read: bad syntax `#f%c['", next);
                      return NULL;
                    } else
                      if (next == 'l')
                        return read_flvector(port, stxsrc, line, col, pos, ch, ']', vector_length, vecbuf, ht, indentation, params, table, 0);
                      else
                        return read_fxvector(port, stxsrc, line, col, pos, ch, ']', vector_length, vecbuf, ht, indentation, params, table, 0);
                    break;
                  case '{':
                    if (!params->curly_braces_are_parens) {
                      scheme_read_err(port, stxsrc, line, col, pos, 2, effective_ch, indentation, "read: bad syntax `#f%c{'", next);
                      return NULL;
                    } else
                      if (next == 'l')
                        return read_flvector(port, stxsrc, line, col, pos, ch, '}', vector_length, vecbuf, ht, indentation, params, table, 0);
                      else
                        return read_fxvector(port, stxsrc, line, col, pos, ch, '}', vector_length, vecbuf, ht, indentation, params, table, 0);
                    break;
                  default:
                    scheme_read_err(port, stxsrc, line, col, pos, 3, effective_ch, indentation,
                                    "read: expected `(' `[' or `{' after #f%c", next);
                  }
                }
            default:
              {
                GC_CAN_IGNORE const mzchar str[] = { 'f', 'a', 'l', 's', 'e', 0 };
                  return read_delimited_constant(ch, str, scheme_false, port, stxsrc, line, col, pos, 
                                                 indentation, params, table);
              }
            }
          }
	case 'c':
	case 'C':
	  {
	    Scheme_Object *v;
	    int sens = 0;
	    int save_sens;

	    ch = scheme_getc_special_ok(port);
	    switch ( ch ) {
	    case 'i':
	    case 'I':
	      sens = 0;
	      break;
	    case 's':
	    case 'S':
	      sens = 1;
	      break;
	    default:
	      scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation,
			      "read: expected `s' or `i' after #c");
	      return NULL;
	    }


	    save_sens = params->case_sensitive;
	    params->case_sensitive = sens;
	    
	    v = read_inner(port, stxsrc, ht, indentation, params, 0);
	    
	    params->case_sensitive = save_sens;
	    
	    if (SCHEME_EOFP(v)) {
	      scheme_read_err(port, stxsrc, line, col, pos, 2, EOF, indentation,
			      "read: end-of-file after #c%c",
			      sens ? 's' : 'i');
	      return NULL;
	    }

	    return v;
	  }
	  break;
	case 's':
	case 'S':
          {
            int orig_ch = ch, effective_ch;
            ch = scheme_getc_special_ok(port);
            if (NOT_EOF_OR_SPECIAL(ch))
              effective_ch = readtable_effective_char(params->table, ch);
            else
              effective_ch = ch;
            if ((orig_ch == 's') 
                && ((effective_ch == '(')
                    || (effective_ch == '[' && params->square_brackets_are_parens)
                    || (effective_ch == '{' && params->curly_braces_are_parens))) {
              Scheme_Object *v;
              Scheme_Struct_Type *st;
              
              if (effective_ch == '(')
                ch = ')';
              else if (effective_ch == '[')
                ch = ']';
              else if (effective_ch == '{')
                ch = '}';

              v = read_vector(port, stxsrc, line, col, pos, orig_ch, ch, -1, NULL, ht, indentation, params, table, 1);
              if (stxsrc)
                v = SCHEME_STX_VAL(v);

              if (SCHEME_VEC_SIZE(v)) {
                Scheme_Object *key;
                key = SCHEME_VEC_ELS(v)[0];
                if (stxsrc)
                  key = scheme_syntax_to_datum(key, 0, NULL);
                st = scheme_lookup_prefab_type(key, SCHEME_VEC_SIZE(v) - 1);
              } else
                st = NULL;

              if (!st || (st->num_slots != (SCHEME_VEC_SIZE(v) - 1))) {
                scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
                                (SCHEME_VEC_SIZE(v)
                                 ? (st
                                    ? ("read: mismatch between structure description"
                                       " and number of provided field values in `#s' form")
                                    : "read: invalid structure description in `#s' form")
                                 : "read: missing structure description in `#s' form"));
                return NULL;
              }

              if (stxsrc && !(MZ_OPT_HASH_KEY(&st->iso) & STRUCT_TYPE_ALL_IMMUTABLE)) {
                scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
                                "read: cannot read mutable `#s' form as syntax");
              }

              v = scheme_make_prefab_struct_instance(st, v);

              if (stxsrc)
                v = scheme_make_stx_w_offset(v, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);

              return v;
            } else {
              scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
                              "read: expected `x'%s after `#%c'",
                              (orig_ch == 's' ? "or `('" : ""),
                              orig_ch);
              return NULL;
            }
          }
	case 'X':
	case 'x': 
          return read_number(-1, port, stxsrc, line, col, pos, 0, 0, 16, 1, ht, indentation, params, table);
	  break;
	case 'B':
	case 'b': 
          return read_number(-1, port, stxsrc, line, col, pos, 0, 0, 2, 1, ht, indentation, params, table);
	  break;
	case 'O':
	case 'o': 
          return read_number(-1, port, stxsrc, line, col, pos, 0, 0, 8, 1, ht, indentation, params, table);
	  break;
	case 'D':
	case 'd': 
          return read_number(-1, port, stxsrc, line, col, pos, 0, 0, 10, 1, ht, indentation, params, table);
	  break;
	case 'E':
	case 'e': 
          return read_number(-1, port, stxsrc, line, col, pos, 0, 1, 10, 0, ht, indentation, params, table);
	  break;
	case 'I':
	case 'i': 
          return read_number(-1, port, stxsrc, line, col, pos, 1, 0, 10, 0, ht, indentation, params, table);
	  break;
	case '\'':
          return read_quote("quoting #'", syntax_symbol, 2, port, stxsrc, line, col, pos, ht, indentation, params);
	  break;
	case '`':
          return read_quote("quasiquoting #`", quasisyntax_symbol, 2, port, stxsrc, line, col, pos, ht, indentation, params);
	  break;
	case ',':
          if (scheme_peekc_special_ok(port) == '@') {
            ch = scheme_getc(port); /* must be '@' */
            return read_quote("unquoting #`@", unsyntax_splicing_symbol, 3, port, stxsrc, line, col, pos, ht, indentation, params);
          } else
            return read_quote("unquoting #`", unsyntax_symbol, 2, port, stxsrc, line, col, pos, ht, indentation, params);
	  break;
	case '~':
          if (params->can_read_compiled) {
            Scheme_Object *cpld;
            cpld = read_compiled(port, stxsrc, line, col, pos, ht, params);
            if (stxsrc)
              cpld = scheme_make_stx_w_offset(cpld, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
            return cpld;
          } else {
            scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation,
                            "read: #~ compiled expressions" NOT_ENABLED_str);
            return NULL;
          }
	  break;
        case '^':
          if (params->read_relative_path) {
            ch = scheme_getc_special_ok(port);
            if (ch == '#') {
              ch = scheme_getc_special_ok(port);
              if (ch == '"') {
                Scheme_Object *str;
                intptr_t sline = 0, scol = 0, spos = 0;

		scheme_tell_all(port, &sline, &scol, &spos);

                str = read_string(1, port, stxsrc, sline, scol, spos, ht, indentation, params, table, 1);

                str->type = SCHEME_PLATFORM_PATH_KIND;

                if (scheme_is_relative_path(SCHEME_PATH_VAL(str), SCHEME_PATH_LEN(str), SCHEME_PLATFORM_PATH_KIND)) {
                  if (SCHEME_PATHP(params->read_relative_path)) {
                    Scheme_Object *a[2];
                    a[0] = params->read_relative_path;
                    a[1] = str;
                    str = scheme_build_path(2, a);
                    a[0] = str;
                    a[1] = scheme_false;
                    str = scheme_simplify_path(2, a);
                  }
                }

                return str;
              }
            } else {
              scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation,
                              "read: bad syntax `#^#%c'",
                              ch);
            }
          } else {
            scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation,
                            "read: bad syntax `#^%c'",
                            ch);
          }
          break;
	case '|':
	  {
	    /* FIXME: integer overflow possible */
	    depth = 0;
	    ch2 = 0;
	    do {
	      ch = scheme_getc_special_ok(port);

	      if (ch == EOF)
		scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, 2), EOF, indentation,
				"read: end of file in #| comment");
	      else if (ch == SCHEME_SPECIAL)
		scheme_get_ready_read_special(port, stxsrc, ht);

	      if ((ch2 == '|') && (ch == '#')) {
		if (!(depth--)) {
		  if ((table && (comment_mode & RETURN_FOR_SPECIAL_COMMENT))
		      || (comment_mode & RETURN_FOR_COMMENT))
		    return NULL;
		  goto start_over;
		}
		ch = 0; /* So we don't count '#' toward an opening "#|" */
	      } else if ((ch2 == '#') && (ch == '|')) {
		depth++;
		ch = 0; /* So we don't count '|' toward a closing "|#" */
	      }
	      ch2 = ch;
	    } while (1);
	  }
	  break;
	case '&':
          if (params->can_read_box)
            return read_box(port, stxsrc, line, col, pos, ht, indentation, params);
          else {
            scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation,
                            "read: #& expressions" NOT_ENABLED_str);
            return NULL;
          }
	  break;
        case 'l':
          {
            mzchar found[5];
            int fl = 1;
            found[0] = 'l';
            ch = scheme_getc_special_ok(port);
            found[fl++] = ch;
	    if (ch == 'a') {
              ch = scheme_getc_special_ok(port);
              found[fl++] = ch;
              if (ch == 'n') {
                ch = scheme_getc_special_ok(port);
                found[fl++] = ch;
                if (ch == 'g') {
                  ch = scheme_getc_special_ok(port);
                  found[fl++] = ch;
                  if (ch == ' ') {
                    /* #lang */
                    Scheme_Object *v;
                    if (!params->can_read_reader
                        || !params->can_read_lang) {
                      scheme_read_err(port, stxsrc, line, col, pos, 6, 0, indentation,
                                      "read: #lang" NOT_ENABLED_str);
                      return NULL;
                    }
                    v = read_lang(port, stxsrc, line, col, pos, get_info, ht, indentation, params, 0);
                    if (!v) {
                      if (comment_mode & RETURN_FOR_SPECIAL_COMMENT)
                        return NULL;
                      goto start_over;
                    }
                    return v;
                  } else {
                    if (ch == EOF) --fl;
                    scheme_read_err(port, stxsrc, line, col, pos, 6, ch, indentation,
                                    "read%s: expected a single space after `#lang'",
                                    (get_info ? "-language" : ""));
                    return NULL;
                  }
                }
              }
            }
            if (ch == EOF) --fl;
            scheme_read_err(port, stxsrc, line, col, pos, fl, ch, indentation,
                            "read%s: bad input: `#%u'",
                            (get_info ? "-language" : ""),
                            found, (intptr_t)fl);
            return NULL;
          }
          break;
	case 'r':
	case 'p':
	  {
	    int orig_ch = ch;
	    int cnt = 0, is_byte = 0;
	    char *expect;

	    ch = scheme_getc_special_ok(port);
	    if (ch == 'x') {
	      expect = "x#";
	      ch = scheme_getc_special_ok(port);
	      cnt++;
	      if (ch == '#') {
		is_byte = 1;
		cnt++;
		ch = scheme_getc_special_ok(port);
	      }
	      if (ch == '"') {
		Scheme_Object *str;
		int is_err;
                intptr_t sline = 0, scol = 0, spos = 0;

		/* Skip #rx[#]: */
		scheme_tell_all(port, &sline, &scol, &spos);

		str = read_string(is_byte, port, stxsrc, sline, scol, spos, ht, indentation, params, table, 1);

		if (stxsrc)
		  str = SCHEME_STX_VAL(str);

		str = scheme_make_regexp(str, is_byte, (orig_ch == 'p'), &is_err);

		if (is_err) {
		  scheme_read_err(port, stxsrc, sline, scol, spos, 2, 0, indentation,
				  "read: bad %sregexp string: %s", 
				  (orig_ch == 'r') ? "" : "p",
				  (char *)str);
		  return NULL;
		}

		if (stxsrc) {
                  str = scheme_intern_literal_string(str);
		  str = scheme_make_stx_w_offset(str, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
                }

		return str;
	      }
	    } else if ((orig_ch == 'r') && (ch == 'e')) {
	      expect = "eader";
	      cnt++;
	      while (expect[cnt]) {
		ch = scheme_getc_special_ok(port);
		if (ch != expect[cnt])
		  break;
		cnt++;
	      }
	      if (!expect[cnt]) {
		/* Found #reader. Read an S-exp. */
		Scheme_Object *v;

		if (!params->can_read_reader) {
		  scheme_read_err(port, stxsrc, line, col, pos, 7, 0, indentation,
				  "read: #reader" NOT_ENABLED_str);
		  return NULL;
		}

		v = read_reader(port, stxsrc, line, col, pos, ht, indentation, params);
		if (!v) {
		  if (comment_mode & RETURN_FOR_SPECIAL_COMMENT)
		    return NULL;
		  goto start_over;
		}
		return v;
	      }
	    } else
	      expect = "";

	    {
	      mzchar a[6];
	      int i;

	      for (i = 0; i < cnt; i++) {
		a[i] = expect[i];
	      }
	      if (NOT_EOF_OR_SPECIAL(ch)) {
		a[cnt++] = ch;
	      }

	      scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos),
			      ch, indentation,
			      "read: bad syntax `#%c%u'",
			      orig_ch, a, (intptr_t)cnt);
	      return NULL;
	    }
	  }
	  break;
	case 'h':
	  {
	    ch = scheme_getc_special_ok(port);
	    if (ch != 'a') {
              scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation,
                              "read: expected `a' after #h");
              return NULL;
	    } else {
	      GC_CAN_IGNORE const mzchar str[] = { 's', 'h', 'e', 'q', 'v', 0 };
	      int scanpos = 0, failed = 0;

	      do {
		ch = scheme_getc_special_ok(port);
		if ((mzchar)ch == str[scanpos]) {
		  scanpos++;
		} else {
		  if ((scanpos == 2) || (scanpos == 4)) {
                    int effective_ch;
                    effective_ch = readtable_effective_char(table, ch);
		    if (!(effective_ch == '(')
			&& !(effective_ch == '[' && params->square_brackets_are_parens)
			&& !(effective_ch == '{' && params->curly_braces_are_parens))
		      failed = 1;
		  } else
		    failed = 1;
		  break;
		}
	      } while (str[scanpos]);
              
	      if (!failed) {
		/* Found recognized tag. Look for open paren... */
                int effective_ch, kind;

		if (scanpos > 4)
		  ch = scheme_getc_special_ok(port);
                
                effective_ch = readtable_effective_char(table, ch);

                if (scanpos == 4)
                  kind = 0;
                else if (scanpos == 2)
                  kind = 1;
                else 
                  kind = 2;

		if (effective_ch == '(')
		  return read_hash(port, stxsrc, line, col, pos, ch, ')', kind, ht, indentation, params, table);
		if (effective_ch == '[' && params->square_brackets_are_parens)
		  return read_hash(port, stxsrc, line, col, pos, ch, ']', kind, ht, indentation, params, table);
		if (effective_ch == '{' && params->curly_braces_are_parens)
		  return read_hash(port, stxsrc, line, col, pos, ch, '}', kind, ht, indentation, params, table);
	      }

	      /* Report an error. So far, we read 'ha', then scanpos chars of str, then ch. */
	      {
		mzchar str_part[7], one_more[2];

		memcpy(str_part, str, scanpos * sizeof(mzchar));
		str_part[scanpos] = 0;
		if (NOT_EOF_OR_SPECIAL(ch)) {
		  one_more[0] = ch;
		  one_more[1] = 0;
		} else
		  one_more[0] = 0;

		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos),
				ch, indentation,
				"read: bad syntax `#ha%5%u'",
				str_part,
				one_more, (intptr_t)(NOT_EOF_OR_SPECIAL(ch) ? 1 : 0));
		return NULL;
	      }
	    }
	  }
	  break;
	case '"':
          return read_string(1, port, stxsrc, line, col, pos, ht, indentation, params, table, 1);
	  break;
	case '<':
          if (scheme_peekc_special_ok(port) == '<') {
            /* Here-string */
            ch = scheme_getc_special_ok(port);
            return read_here_string(port, stxsrc, line, col, pos,indentation, params);
          } else {
            scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation, "read: bad syntax `#<'");
            return NULL;
          }
	  break;
        case '!':
          ch = scheme_getc_special_ok(port);
          if ((ch == ' ') || (ch == '/')) {
            /* line comment, with '\' as a continuation */
            int was_backslash = 0, was_backslash_cr = 0;
            while(1) {
              was_backslash_cr = 0;
              ch = scheme_getc_special_ok(port);
              if (ch == EOF) {
                break;
              } else if (ch == SCHEME_SPECIAL) {
		scheme_get_ready_read_special(port, stxsrc, ht);
              } else if (ch == '\r') {
                if (was_backslash) {
                  was_backslash_cr = 1;
                } else
                  break;
              } else if (ch == '\n') {
                if (!was_backslash && !was_backslash_cr)
                  break;
              }
              was_backslash = (ch == '\\');
            }
            if (comment_mode & RETURN_FOR_COMMENT)
              return NULL;
            goto start_over;
          } else if ((ch < 128) && is_lang_nonsep_char(ch)) {
            Scheme_Object *v;
            if (!params->can_read_reader
                || !params->can_read_lang) {
              scheme_read_err(port, stxsrc, line, col, pos, 2, 0, indentation,
                              "read: #!" NOT_ENABLED_str);
              return NULL;
            }
            v = read_lang(port, stxsrc, line, col, pos, get_info, ht, indentation, params, ch);
            if (!v) {
              if (comment_mode & RETURN_FOR_SPECIAL_COMMENT)
                return NULL;
              goto start_over;
            }
            return v;
          } else {
            if (NOT_EOF_OR_SPECIAL(ch))
              scheme_read_err(port, stxsrc, line, col, pos, 3, 
                              ch, indentation, "read: bad syntax `#!%c'", ch);
            else
              scheme_read_err(port, stxsrc, line, col, pos, 2, 
                              ch, indentation, "read: bad syntax `#!'", ch);
            return NULL;
          }
          break;
	default:
          {
	    int vector_length = -1;
	    int overflow = 0, digits = 0, effective_ch;
	    mzchar tagbuf[64], vecbuf[64]; /* just for errors */
            effective_ch = read_vector_length(port, table, &ch, tagbuf, vecbuf, &vector_length, &digits, &overflow);

	    if (effective_ch == '(')
	      return read_vector(port, stxsrc, line, col, pos, ch, ')', vector_length, vecbuf, ht, indentation, params, table, 0);
	    if (effective_ch == '[' && params->square_brackets_are_parens)
	      return read_vector(port, stxsrc, line, col, pos, ch, ']', vector_length, vecbuf, ht, indentation, params, table, 0);
	    if (effective_ch == '{' && params->curly_braces_are_parens)
	      return read_vector(port, stxsrc, line, col, pos, ch, '}', vector_length, vecbuf, ht, indentation, params, table, 0);

	    if (ch == '#' && (vector_length != -1)) {
	      /* Not a vector after all: a graph reference */
	      Scheme_Object *ph;

	      if (stxsrc)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: #..# expressions not allowed in read-syntax mode");

	      if (!params->can_read_graph)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: #..# expressions" NOT_ENABLED_str);

	      if (digits > MAX_GRAPH_ID_DIGITS)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: graph id too long in #%5#",
				tagbuf);

	      if (*ht)
		ph = (Scheme_Object *)scheme_hash_get(*ht, scheme_make_integer(vector_length));
	      else
		ph = NULL;

	      if (!ph) {
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: no #%d= preceding #%d#",
				vector_length, vector_length);
		return scheme_void;
	      }
	      return ph;
	    }
	    if (ch == '=' && (vector_length != -1)) {
	      /* Not a vector after all: a graph definition */
	      Scheme_Object *v, *ph;
              intptr_t in_pos;

	      if (stxsrc)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: #..= expressions not allowed in read-syntax mode");

	      if (!params->can_read_graph)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				 "read: #..= expressions" NOT_ENABLED_str);

	      if (digits > MAX_GRAPH_ID_DIGITS)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				 "read: graph id too long in #%s=",
				 tagbuf);

	      if (*ht) {
		if (scheme_hash_get(*ht, scheme_make_integer(vector_length))) {
		  scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				  "read: multiple #%d= tags",
				  vector_length);
		  return NULL;
		}
	      } else {
		Scheme_Hash_Table *tht;
		tht = scheme_make_hash_table(SCHEME_hash_ptr);
		*ht = tht;
	      }
	      ph = scheme_alloc_small_object();
	      ph->type = scheme_placeholder_type;

	      scheme_hash_set(*ht, scheme_make_integer(vector_length), (void *)ph);

              scheme_tell_all(port, NULL, NULL, &in_pos);

	      v = read_inner(port, stxsrc, ht, indentation, params, 0);
	      if (SCHEME_EOFP(v))
		scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, in_pos-pos), EOF, indentation,
				"read: expected an element for graph (found end-of-file)");
	      SCHEME_PTR_VAL(ph) = v;

	      return v;
	    }

	    {
	      char *lbuffer;
	      int pch = ch, ulen, blen;

	      if ((pch == EOF) || (pch == SCHEME_SPECIAL))
		pch = 0;

	      ulen = scheme_char_strlen(tagbuf);
	      blen = scheme_utf8_encode_all(tagbuf, ulen, NULL);
	      lbuffer = (char *)scheme_malloc_atomic(blen + MAX_UTF8_CHAR_BYTES + 1);
	      scheme_utf8_encode_all(tagbuf, ulen, (unsigned char *)lbuffer);
	      blen += scheme_utf8_encode((mzchar *)&pch, 0, 1,
					 (unsigned char *)lbuffer, blen,
					 0);
	      lbuffer[blen] = 0;

	      scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
			      "read: bad syntax `#%s'",
			      lbuffer);

	      return NULL;
	    }
	  }
	  break;
	}
      break;
    default:
      if (isdigit_ascii(ch))
	special_value = read_number(ch, port, stxsrc, line, col, pos, 0, 0, 10, 0, ht, indentation, params, table);
      else
	special_value = read_symbol(ch, 0, port, stxsrc, line, col, pos, ht, indentation, params, table);
      break;
    }

  /* We get here after reading a "symbol". Check for a comment. */
  {
    Scheme_Object *v = special_value;

    if (scheme_special_comment_value(v)) {
      /* a "comment" */
      if (comment_mode & RETURN_FOR_SPECIAL_COMMENT)
	return NULL;
      else {
	special_value_need_copy = 0;
	goto start_over;
      }
    } else if (SCHEME_STXP(v)) {
      if (!stxsrc)
	v = scheme_syntax_to_datum(v, 0, NULL);
    } else if (stxsrc) {
      Scheme_Object *s;
      s = scheme_make_stx_w_offset(scheme_false, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
      v = scheme_datum_to_syntax(v, s, scheme_false, 1, 0);
    }
    if (special_value_need_copy && !stxsrc) {
      set_need_copy(ht);
    }
    return v;
  }
}

static Scheme_Object *
read_inner_inner(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht,
		 Scheme_Object *indentation, ReadParams *params,
		 int comment_mode, int pre_char, Readtable *table,
         int get_info)
{
  intptr_t rline = 0, rcol = 0, rpos = 0;
  intptr_t dline = 0, dcol = 0, dpos = 0;
  Scheme_Object *ret;
  int read_cdot, next, found_dot;

  read_cdot = params->read_cdot;
  
  scheme_tell_all(port, &rline, &rcol, &rpos);
  ret = read_inner_inner_inner(port, stxsrc, ht, indentation, params, comment_mode, pre_char, table, get_info);

  if (!read_cdot) { return ret; }

  found_dot = 0;
  while ( 1 ) {    
    next = scheme_peekc_special_ok(port);
    if ( next == EOF ) { break; }
    if ( (table && readtable_kind(table, next, params) & READTABLE_WHITESPACE)
         || (!table && scheme_isspace(next)) ) {
      scheme_getc_special_ok(port); continue; }
    if ( (table && readtable_effective_char(table, next) == '.')
         || (!table && next == '.') ) {
      scheme_getc_special_ok(port); found_dot = 1; break; }
    break;
  }

  if ( found_dot ) {
    Scheme_Object *dot, *next;
    scheme_tell_all(port, &dline, &dcol, &dpos);
    dot = dot_symbol;
    if (stxsrc) {
      dot = scheme_make_stx_w_offset(dot, dline, dcol, dpos, SPAN(port,dpos), stxsrc, STX_SRCTAG);
    }
    next = read_inner_inner(port, stxsrc, ht, indentation, params, comment_mode, pre_char, table, get_info);
    ret = scheme_make_pair( dot, scheme_make_pair( ret, scheme_make_pair( next, scheme_null ) ) );
    if (stxsrc) {
      ret = scheme_make_stx_w_offset(ret, rline, rcol, rpos, SPAN(port,rpos), stxsrc, STX_SRCTAG);
    }
  }
  
  return ret;
}

static Scheme_Object *
read_inner(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht,
	   Scheme_Object *indentation, ReadParams *params,
	   int comment_mode)
{
  return read_inner_inner(port, stxsrc, ht, indentation, params, comment_mode, -1, params->table, 0);
}

#ifdef DO_STACK_CHECK
static Scheme_Object *resolve_references(Scheme_Object *obj,
					 Scheme_Object *port,
					 Scheme_Object *top,
                                         Scheme_Hash_Table *dht,
                                         Scheme_Hash_Table *tht,
                                         Scheme_Hash_Table *self_contained_ht,
                                         int clone,
                                         int tail_depth);

static Scheme_Object *resolve_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *port = (Scheme_Object *)p->ku.k.p2;
  Scheme_Object *top = (Scheme_Object *)p->ku.k.p5;
  Scheme_Hash_Table *dht = (Scheme_Hash_Table *)p->ku.k.p3;
  Scheme_Hash_Table *tht = (Scheme_Hash_Table *)SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  Scheme_Hash_Table *self_contained_ht = (Scheme_Hash_Table *)SCHEME_CDR((Scheme_Object *)p->ku.k.p4);

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  return resolve_references(o, port, top, dht, tht, self_contained_ht, p->ku.k.i1, p->ku.k.i2);
}
#endif

static Scheme_Object *resolve_references(Scheme_Object *obj,
					 Scheme_Object *port,
					 Scheme_Object *top,
					 Scheme_Hash_Table *dht,
					 Scheme_Hash_Table *tht,
                                         Scheme_Hash_Table *self_contained_ht,
                                         int clone,
                                         int tail_depth)
{
  Scheme_Object *result;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)obj;
      p->ku.k.p2 = (void *)port;
      p->ku.k.p5 = (void *)top;
      p->ku.k.p3 = (void *)dht;
      result = scheme_make_pair((Scheme_Object *)tht,
                                (Scheme_Object *)self_contained_ht);
      p->ku.k.p4 = (void *)result;
      p->ku.k.i1 = clone;
      p->ku.k.i2 = tail_depth;
      return scheme_handle_stack_overflow(resolve_k);
    }
  }
#endif

  SCHEME_USE_FUEL(1);

  if (SAME_TYPE(SCHEME_TYPE(obj), scheme_placeholder_type)) {
    Scheme_Object *start = obj;
    while (SAME_TYPE(SCHEME_TYPE(obj), scheme_placeholder_type)) {
      obj = (Scheme_Object *)SCHEME_PTR_VAL(obj);
      if (SAME_OBJ(start, obj)) {
        if (port)
          scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                          "read: illegal placeholder cycle");
        else {
          scheme_contract_error("make-reader-graph",
                                "illegal placeholder cycle in value",
                                "value", 1, top,
                                NULL);
        }
        return NULL;
      }
    }
  }

  if (self_contained_ht
      && scheme_hash_get(self_contained_ht, obj))
    return obj;

  result = scheme_hash_get(dht, obj);
  if (result) {
    if (SCHEME_PAIRP(result)) {
      obj = scheme_hash_get(tht, result);
      if (obj && (SCHEME_INT_VAL(obj) == tail_depth))
        SCHEME_PAIR_FLAGS(result) |= PAIR_IS_NON_LIST;
    }
    return result;
  }

  result = obj;

  if (SCHEME_PAIRP(obj)) {
    Scheme_Object *rr;

    if (clone)
      result = scheme_make_pair(scheme_false, scheme_false);
    scheme_hash_set(dht, obj, result);

    rr = resolve_references(SCHEME_CAR(obj), port, top, dht, tht, self_contained_ht,
                            clone, tail_depth + 1);
    SCHEME_CAR(result) = rr;

    scheme_hash_set(tht, result, scheme_make_integer(tail_depth));

    rr = resolve_references(SCHEME_CDR(obj), port, top, dht, tht, self_contained_ht,
                            clone, tail_depth);
    SCHEME_CDR(result) = rr;

    scheme_hash_set(tht, result, NULL);

    if (clone
        && SAME_OBJ(SCHEME_CAR(obj), SCHEME_CAR(result))
        && SAME_OBJ(SCHEME_CDR(obj), SCHEME_CDR(result))) {
      /* No changes, so we don't actually have to clone. */
      result = obj;
      scheme_hash_set(dht, obj, result);
    }
  } else if (SCHEME_BOXP(obj)) {
    Scheme_Object *rr;

    if (clone) {
      result = scheme_box(scheme_false);
      if (SCHEME_IMMUTABLEP(obj))
        SCHEME_SET_IMMUTABLE(result);
    }
    scheme_hash_set(dht, obj, result);

    rr = resolve_references(SCHEME_BOX_VAL(obj), port, top, dht, tht, self_contained_ht,
                            clone, tail_depth + 1);
    SCHEME_BOX_VAL(result) = rr;

    if (clone
        && SAME_OBJ(SCHEME_PTR_VAL(obj), SCHEME_PTR_VAL(result))) {
      result = obj;
      scheme_hash_set(dht, obj, result);
    }
  } else if (SCHEME_VECTORP(obj)
             || (clone && SCHEME_CHAPERONE_VECTORP(obj))) {
    int i, len, diff = 0;
    Scheme_Object *prev_rr, *prev_v;

    if (SCHEME_NP_CHAPERONEP(obj))
      obj = scheme_chaperone_vector_copy(obj);

    len = SCHEME_VEC_SIZE(obj);

    if (clone) {
      result = scheme_make_vector(len, scheme_false);
      if (SCHEME_IMMUTABLEP(obj))
        SCHEME_SET_IMMUTABLE(result);
    }
    scheme_hash_set(dht, obj, result);

    prev_v = prev_rr = NULL;
    for (i = 0; i < len; i++) {
      Scheme_Object *rr;
      if (SCHEME_VEC_ELS(obj)[i] == prev_v) {
	rr = prev_rr;
      } else {
	prev_v = SCHEME_VEC_ELS(obj)[i];
	rr = resolve_references(prev_v, port, top, dht, tht, self_contained_ht,
                                clone, tail_depth + 1);
        if (!SAME_OBJ(prev_v, rr))
          diff = 1;
	prev_rr = rr;
      }
      SCHEME_VEC_ELS(result)[i] = rr;
    }

    if (clone && !diff) {
      result = obj;
      scheme_hash_set(dht, obj, result);
    }
  } else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_table_placeholder_type)
             || SCHEME_HASHTRP(obj)
             || (clone && SCHEME_NP_CHAPERONEP(obj) 
                 && (SCHEME_HASHTP(SCHEME_CHAPERONE_VAL(obj))
                     || SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(obj))))) {
    Scheme_Hash_Tree *t, *base;
    Scheme_Object *a, *key, *val, *lst;
    int kind;

    if (SCHEME_NP_CHAPERONEP(obj))
      obj = scheme_chaperone_hash_table_copy(obj);

    if (SCHEME_HASHTRP(obj)) {
      mzlonglong i;
      if (scheme_is_hash_tree_equal(obj))
        kind = 1;
      else if (scheme_is_hash_tree_eqv(obj))
        kind = 2;
      else
        kind = 0;
      t = (Scheme_Hash_Tree *)obj;
      lst = scheme_null;
      for (i = scheme_hash_tree_next(t, -1); i != -1; i = scheme_hash_tree_next(t, i)) {
        scheme_hash_tree_index(t, i, &key, &val);
        lst = scheme_make_pair(scheme_make_pair(key, val), lst);
      }
    } else {
      kind = SCHEME_PINT_VAL(obj);
      lst = SCHEME_IPTR_VAL(obj);
    }

    /* Create `t' to be overwritten, and create `base' to extend. */
    base = scheme_make_hash_tree(kind);
    if (SCHEME_NULLP(lst))
      t = base;
    else
      t = scheme_make_hash_tree_placeholder(kind);

    result = (Scheme_Object *)t;
    scheme_hash_set(dht, obj, result);

    lst = resolve_references(lst, port, top, dht, tht, self_contained_ht,
                             clone, tail_depth + 1);

    for (; SCHEME_PAIRP(lst); lst = SCHEME_CDR(lst)) {
      a = SCHEME_CAR(lst);
      key = SCHEME_CAR(a);
      val = SCHEME_CDR(a);
     
      base = scheme_hash_tree_set(base, key, val);
    }

    if (base->count)
      scheme_hash_tree_tie_placeholder(t, base);
  } else if (SCHEME_HASHTP(obj)) {
    int i;
    Scheme_Object *key, *val, *l = scheme_null, *orig_l;
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)obj, *t2;

    t2 = scheme_clone_hash_table(t);
    scheme_reset_hash_table(t2, NULL);
    result = (Scheme_Object *)t2;

    scheme_hash_set(dht, obj, (Scheme_Object *)t2);
    
    for (i = t->size; i--; ) {
      if (t->vals[i]) {
        key = t->keys[i];
        val = t->vals[i];
        l = scheme_make_pair(scheme_make_pair(key, val), l);
      }
    }

    orig_l = l;
    l = resolve_references(l, port, top, dht, tht, self_contained_ht,
                           clone, tail_depth + 1);

    if (SAME_OBJ(l, orig_l)) {
      result = obj;
      scheme_hash_set(dht, obj, result);
    } else {
      for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
        val = SCHEME_CAR(l);
        key = SCHEME_CAR(val);
        val = SCHEME_CDR(val);
        
        scheme_hash_set(t2, key, val);
      }
    }
  } else if (SCHEME_STRUCTP(obj)
             || (clone && SCHEME_CHAPERONE_STRUCTP(obj))) {
    Scheme_Structure *s;
    if (clone && SCHEME_CHAPERONEP(obj))
      s = (Scheme_Structure *)SCHEME_CHAPERONE_VAL(obj);
    else
      s = (Scheme_Structure *)obj;
    if (s->stype->prefab_key) {
      /* prefab */
      int c, i, diff;
      Scheme_Object *prev_v, *v;

      if (clone) {
        result = scheme_clone_prefab_struct_instance((Scheme_Structure *)obj);
      }
      scheme_hash_set(dht, obj, result);

      c = s->stype->num_slots;
      diff = 0;
      for (i = 0; i < c; i++) {
        prev_v = ((Scheme_Structure *)result)->slots[i];
	v = resolve_references(prev_v, port, top, dht, tht, self_contained_ht,
                               clone, tail_depth + 1);
        if (!SAME_OBJ(prev_v, v))
          diff = 1;
        ((Scheme_Structure *)result)->slots[i] = v;
      }

      if (clone && !diff) {
        result = obj;
        scheme_hash_set(dht, obj, result);
      }
    }
  }

  return result;
}

static Scheme_Object *
_internal_read(Scheme_Object *port, Scheme_Object *stxsrc, int crc, int cant_fail,
               int recur, int expose_comment, int extra_char,
               Scheme_Object *init_readtable,
               Scheme_Object *magic_sym, Scheme_Object *magic_val,
               Scheme_Object *delay_load_info, int get_info)
{
  Scheme_Object *v, *v2;
  Scheme_Config *config;
  Scheme_Hash_Table **ht = NULL;
  ReadParams params;

  config = scheme_current_config();

  if (get_info) {
    params.table = NULL;
  } else {
    v = scheme_get_param(config, MZCONFIG_READTABLE);
    if (SCHEME_TRUEP(v))
      params.table = (Readtable *)v;
    else
      params.table = NULL;
  }
  if (crc >= 0) {
    params.can_read_compiled = crc;
    params.can_read_unsafe = 1;
  } else {
    v = scheme_get_param(scheme_current_config(), MZCONFIG_CAN_READ_COMPILED);
    params.can_read_compiled = SCHEME_TRUEP(v);
    if (v) {
      v = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
      v2 = scheme_get_initial_inspector();
      params.can_read_unsafe = SAME_OBJ(v, v2);
    } else
      params.can_read_unsafe = 0;
  }
  v = scheme_get_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE);
  params.can_read_pipe_quote = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CAN_READ_BOX);
  params.can_read_box = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CAN_READ_GRAPH);
  params.can_read_graph = SCHEME_TRUEP(v);
  if ((crc > 0) || get_info) {
    params.can_read_reader = 1;
    params.can_read_lang = 1;
  } else {
    v = scheme_get_param(config, MZCONFIG_CAN_READ_READER);
    params.can_read_reader = SCHEME_TRUEP(v);
    v = scheme_get_param(config, MZCONFIG_CAN_READ_LANG);
    params.can_read_lang = SCHEME_TRUEP(v);
  }
  v = scheme_get_param(config, MZCONFIG_CASE_SENS);
  params.case_sensitive = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS);
  params.square_brackets_are_parens = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CURLY_BRACES_ARE_PARENS);
  params.curly_braces_are_parens = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_SQUARE_BRACKETS_ARE_TAGGED);
  params.square_brackets_are_tagged = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CURLY_BRACES_ARE_TAGGED);
  params.curly_braces_are_tagged = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_READ_CDOT);
  params.read_cdot = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_READ_DECIMAL_INEXACT);
  params.read_decimal_inexact = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CAN_READ_QUASI);
  params.can_read_quasi = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CAN_READ_DOT);
  params.can_read_dot = SCHEME_TRUEP(v);
  v = scheme_get_param(config, MZCONFIG_CAN_READ_INFIX_DOT);
  params.can_read_infix_dot = SCHEME_TRUEP(v);
  params.read_relative_path = NULL;
  if (!delay_load_info)
    delay_load_info = scheme_get_param(config, MZCONFIG_DELAY_LOAD_INFO);
  if (SCHEME_TRUEP(delay_load_info))
    params.delay_load_info = delay_load_info;
  else
    params.delay_load_info = NULL;
  params.skip_zo_vers_check = cant_fail;
  params.magic_sym = magic_sym;
  params.magic_val = magic_val;

  ht = NULL;
  if (recur) {
    /* Check whether this is really a recursive call. If so,
       we get a pointer to a hash table for cycles: */
    v = scheme_extract_one_cc_mark(NULL, unresolved_uninterned_symbol);
    if (v && SCHEME_RPAIRP(v)) {
      if (SCHEME_FALSEP(SCHEME_CDR(v)) == !stxsrc)
	ht = (Scheme_Hash_Table **)SCHEME_CAR(v);
    }
  }
  if (!ht) {
    ht = MALLOC_N(Scheme_Hash_Table *, 1);
    recur = 0;
  }

  do {
    v = read_inner_inner(port, stxsrc, ht, scheme_null, &params, 
			 (RETURN_FOR_HASH_COMMENT 
			  | (expose_comment ? (RETURN_FOR_COMMENT | RETURN_FOR_SPECIAL_COMMENT) : 0)),
			 extra_char, 
			 (init_readtable 
			  ? (SCHEME_FALSEP(init_readtable)
			     ? NULL
			     : (Readtable *)init_readtable)
			  : params.table),
                         get_info);

    extra_char = -1;

    if (*ht && !recur) {
      /* Resolve placeholders: */
      int clone = 0;
      Scheme_Hash_Table *dht, *tht;

      if (stxsrc)
        scheme_signal_error("internal error: read-syntax has graph references");

      /* If we ever called an external reader, 
         then we need to clone everything. */
      if (scheme_hash_get(*ht, tainted_uninterned_symbol))
        clone = 1;

      dht = scheme_make_hash_table(SCHEME_hash_ptr);
      tht = scheme_make_hash_table(SCHEME_hash_ptr);

      if (v)
	v = resolve_references(v, port, NULL, dht, tht, NULL, clone, 0);

      /* In case some placeholders were introduced by #;: */
      v2 = scheme_hash_get(*ht, unresolved_uninterned_symbol);
      if (v2)
	resolve_references(v2, port, NULL, dht, tht, NULL, clone, 0);

      if (!v)
	*ht = NULL;
    }

    if (!v && expose_comment) {
      /* Return to indicate comment: */
      v = scheme_alloc_small_object();
      v->type = scheme_special_comment_type;
      SCHEME_PTR_VAL(v) = scheme_false;
      return v;
    }
  } while (!v);

  return v;
}

static void *scheme_internal_read_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *stxsrc = (Scheme_Object *)p->ku.k.p2;
  Scheme_Object *init_readtable = (Scheme_Object *)p->ku.k.p3;
  Scheme_Object *magic_sym = (Scheme_Object *)p->ku.k.p4;
  Scheme_Object *magic_val = NULL;
  Scheme_Object *delay_load_info = (Scheme_Object *)p->ku.k.p5;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  if (magic_sym) {
    magic_val = SCHEME_CDR(magic_sym);
    magic_sym = SCHEME_CAR(magic_sym);
  }

  return (void *)_internal_read(port, stxsrc, p->ku.k.i1, 0,
                                p->ku.k.i3 & 0x2, p->ku.k.i3 & 0x1, 
                                p->ku.k.i4, init_readtable,
                                magic_sym, magic_val, delay_load_info, 0);
}

Scheme_Object *
scheme_internal_read(Scheme_Object *port, Scheme_Object *stxsrc, int crc, int cantfail,
		     int recur, int expose_comment, int pre_char,
                     Scheme_Object *init_readtable,
		     Scheme_Object *magic_sym, Scheme_Object *magic_val,
                     Scheme_Object *delay_load_info)
{
  Scheme_Thread *p = scheme_current_thread;

  if (cantfail) {
    return _internal_read(port, stxsrc, crc, cantfail, recur, expose_comment, -1, NULL,
                          magic_sym, magic_val, delay_load_info, 0);
  } else {
    if (magic_sym)
      magic_sym = scheme_make_pair(magic_sym, magic_val);

    p->ku.k.p1 = (void *)port;
    p->ku.k.p2 = (void *)stxsrc;
    p->ku.k.i1 = crc;
    p->ku.k.i3 = ((recur ? 0x2 : 0) | (expose_comment ? 0x1 : 0));
    p->ku.k.i4 = pre_char;
    p->ku.k.p3 = (void *)init_readtable;
    p->ku.k.p4 = (void *)magic_sym;
    p->ku.k.p5 = (void *)delay_load_info;

    return (Scheme_Object *)scheme_top_level_do(scheme_internal_read_k, 0);
  }
}

Scheme_Object *scheme_read(Scheme_Object *port)
{
  return scheme_internal_read(port, NULL, -1, 0, 0, 0, -1, NULL, NULL, NULL, NULL);
}

Scheme_Object *scheme_read_syntax(Scheme_Object *port, Scheme_Object *stxsrc)
{
  return scheme_internal_read(port, stxsrc, -1, 0, 0, 0, -1, NULL, NULL, NULL, NULL);
}

Scheme_Object *scheme_resolve_placeholders(Scheme_Object *obj)
{
  return resolve_references(obj, NULL, obj, 
                            scheme_make_hash_table(SCHEME_hash_ptr),
                            scheme_make_hash_table(SCHEME_hash_ptr),
                            NULL,
                            1, 0);
}

/*========================================================================*/
/*                             list reader                                */
/*========================================================================*/

static Scheme_Object *attach_shape_property(Scheme_Object *list, 
					    Scheme_Object *stxsrc, 
					    ReadParams *params, 
					    int closer);

static Scheme_Object *attach_shape_tag(Scheme_Object *list,
                        intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
					    Scheme_Object *stxsrc, 
					    ReadParams *params, 
					    int closer);

static int next_is_delim(Scheme_Object *port,
			 ReadParams *params,
			 int brackets,
			 int braces)
{
  int next;
  next = scheme_peekc_special_ok(port);
  return ((next == EOF)
	  || (next == SCHEME_SPECIAL)
	  || (!params->table 
	      && (scheme_isspace(next)
		  || (next == '(')
		  || (next == ')')
		  || (next == '"')
		  || (next == ';')
		  || (next == '\'')
		  || (next == '`')
		  || (next == ',')
		  || ((next == '[') && brackets)
		  || ((next == '{') && braces)
		  || ((next == ']') && brackets)
		  || ((next == '}') && braces)))
	  || (params->table 
	      && (readtable_kind(params->table, next, params) 
		  & (READTABLE_WHITESPACE | READTABLE_TERMINATING))));
}

static const char *mapping_name(ReadParams *params, int ch, const char *def, int name_pos)
{
  if (params->table) {
    int i;
    char *buf = "";
    Scheme_Object *v;
    Scheme_Hash_Table *mapping;

    if (params->table->names) {
      if (params->table->names[name_pos])
        return params->table->names[name_pos];
    }

    mapping = params->table->mapping;
    if (!scheme_hash_get(mapping, scheme_make_integer(ch))) {
      buf = (char *)scheme_malloc_atomic(4);
      sprintf(buf, "`%c'", ch);
    }

    for (i = mapping->size; i--; ) {
      if (mapping->vals[i]) {
        v = mapping->vals[i];
        if ((SCHEME_INT_VAL(SCHEME_CAR(v)) == READTABLE_MAPPED)
            && (SCHEME_INT_VAL(SCHEME_CDR(v)) == ch)) {
          int len;
          mzchar a[2];
          char *naya, utf8_buf[MAX_UTF8_CHAR_BYTES + 1];

          v = mapping->keys[i];
          a[0] = (mzchar)SCHEME_INT_VAL(v);
          len = scheme_utf8_encode_all(a, 1, (unsigned char *)utf8_buf);
          utf8_buf[len] = 0;

          naya = (char *)scheme_malloc_atomic(len + 5 + strlen(buf));
          sprintf(naya, "`%s'", utf8_buf);
          if (*buf) {
            sprintf(naya XFORM_OK_PLUS len + 2, " or %s", buf);
          }
          buf = naya;
        }
      }
    }

    if (!params->table->names) {
      char **a;
      a = MALLOC_N(char*, 7);
      params->table->names = a;
    }
    params->table->names[name_pos] = buf;

    return buf;
  } else
    return def;
}

static const char *closer_name(ReadParams *params, int closer)
{
  int pos;
  const char *def;

  switch (closer) {
  case ')':
    pos = 0;
    def = "`)'";
    break;
  case ']':
    pos = 1;
    def = "`]'";
    break;
  case '}':
  default:
    pos = 2;
    def = "`}'";
    break;
  }

  return mapping_name(params, closer, def, pos);
}

static const char *opener_name(ReadParams *params, int opener)
{
  int pos;
  const char *def;

  switch (opener) {
  case '(':
    pos = 3;
    def = "`('";
    break;
  case '[':
    pos = 4;
    def = "`['";
    break;
  case '{':
  default:
    pos = 5;
    def = "`{'";
    break;
  }

  return mapping_name(params, opener, def, pos);
}

static const char *dot_name(ReadParams *params)
{
  return mapping_name(params, '.', "`.'", 6);
}

/* "(" (or other opener) has already been read */
static Scheme_Object *
read_list(Scheme_Object *port,
	  Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	  int opener, int closer, int shape, int use_stack,
	  Scheme_Hash_Table **ht,
	  Scheme_Object *indentation,
	  ReadParams *params, Readtable *table)
{
  Scheme_Object *list = NULL, *last = NULL, *car, *cdr, *pair, *infixed = NULL, *prefetched = NULL;
  int ch = 0, got_ch_already = 0, effective_ch;
  int brackets = params->square_brackets_are_parens || params->square_brackets_are_tagged;
  int braces = params->curly_braces_are_parens || params->curly_braces_are_tagged;
  intptr_t start, startcol, startline, dotpos, dotcol, dotline, dot2pos, dot2line, dot2col, init_span;

  scheme_tell_all(port, &startline, &startcol, &start);
  init_span = 1;

  if (stxsrc) {
    /* Push onto the indentation stack: */
    Scheme_Indent *indt;
    indt = (Scheme_Indent *)scheme_malloc_atomic_tagged(sizeof(Scheme_Indent));
    indt->type = scheme_indent_type;

    indt->closer = closer;
    indt->max_indent = startcol + 1;
    indt->multiline = 0;
    indt->suspicious_line = 0;
    indt->suspicious_quote = 0;
    indt->start_line = startline;
    indt->last_line = startline;

    indentation = scheme_make_pair((Scheme_Object *)indt, indentation);
  }

  while (1) {
    if (prefetched)
      ch = 0;
    else if (got_ch_already)
      got_ch_already = 0;
    else
      ch = skip_whitespace_comments(port, stxsrc, ht, indentation, params, table, NULL);

    if ((ch == EOF) && (closer != EOF)) {
      char *suggestion = "";
      if (SCHEME_PAIRP(indentation)) {
	Scheme_Indent *indt;

	indt = (Scheme_Indent *)SCHEME_CAR(indentation);
	if (indt->suspicious_line) {
	  suggestion = scheme_malloc_atomic(100);
	  sprintf(suggestion,
		  "\n  possible cause: indentation suggests a missing %s before line %" PRIdPTR,
		  closer_name(params, indt->suspicious_closer),
		  indt->suspicious_line);
	}
      }

      scheme_read_err(port, stxsrc, startline, startcol, start, MINSPAN(port, start, init_span), EOF, indentation,
		      "read: expected a %s to close `%c'%s", 
                      closer_name(params, closer), 
                      opener,
                      suggestion);
      return NULL;
    }

    effective_ch = readtable_effective_char(table, ch);

    if (effective_ch == closer) {
      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
			"read: expected hash pair (with key and value separated by %s) before `%c'",
                        dot_name(params),
			ch);
	return NULL;
      }
      if (!list) list = scheme_null;
      pop_indentation(indentation);
      list = attach_shape_tag(list, line, col, pos, SPAN(port, pos), stxsrc, params, closer);
      list = (stxsrc
	      ? scheme_make_stx_w_offset(list, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG)
	      : list);
      list = attach_shape_property(list, stxsrc, params, closer);
      return list;
    }

    if (shape == mz_shape_hash_list) {
      /* Make sure we found a parenthesized something. */
      if (!(effective_ch == '(')
	  && !(effective_ch == '[' && params->square_brackets_are_parens)
	  && !(effective_ch == '{' && params->curly_braces_are_parens)) {
	intptr_t xl, xc, xp;
        const char *sbname, *cbname;

	/* If it's a special or we have a readtable, we need to read ahead
	   to make sure that it's not a comment. For consistency, always
	   read ahead. */
	scheme_ungetc(ch, port);
	prefetched = read_inner(port, stxsrc, ht, indentation, params, 
                                RETURN_FOR_SPECIAL_COMMENT);
	if (!prefetched)
	  continue; /* It was a comment; try again. */

        sbname = (params->square_brackets_are_parens ? opener_name(params, '[') : "");
        cbname = (params->curly_braces_are_parens ? opener_name(params, '{') : "");

        scheme_tell_all(port, &xl, &xc, &xp);
	scheme_read_err(port, stxsrc, xl, xc, xp, 1,
			ch, indentation,
			"read: expected %s%s%s%s%s to start a hash pair",
                        opener_name(params, '('),
                        params->square_brackets_are_parens ? " or " : "",
                        sbname,
                        params->curly_braces_are_parens ? " or " : "",
                        cbname);
	return NULL;
      } else {
	/* Found paren. Use read_list directly so we can specify mz_shape_hash_elem. */
	intptr_t xl, xc, xp;
	scheme_tell_all(port, &xl, &xc, &xp);
	car = read_list(port, stxsrc, xl, xc, xp,
			ch, ((effective_ch == '(') ? ')' : ((effective_ch == '[') ? ']' : '}')),
			mz_shape_hash_elem, use_stack, ht, indentation, params, table);
	/* car is guaranteed to have an appropriate shape */
      }
    } else {
      if (prefetched) {
	car = prefetched;
	prefetched = NULL;
      } else {
	scheme_ungetc(ch, port);
        switch (shape) {
          case mz_shape_fl_vec:
            car = read_flonum(port, NULL, ht, indentation, params, RETURN_FOR_SPECIAL_COMMENT);
            MZ_ASSERT(SCHEME_DBLP(car));
            break;
          case mz_shape_fx_vec:
            car = read_fixnum(port, NULL, ht, indentation, params, RETURN_FOR_SPECIAL_COMMENT);
            MZ_ASSERT(SCHEME_INTP(car));
            break;
          default:
	    car = read_inner(port, stxsrc, ht, indentation, params, RETURN_FOR_SPECIAL_COMMENT);
        }
	if (!car) continue; /* special was a comment */
      }
      /* can't be eof, due to check above */
    }

    pair = scheme_make_pair(car, scheme_null);

  retry_before_dot:

    ch = skip_whitespace_comments(port, stxsrc, ht, indentation, params, table, NULL);
    effective_ch = readtable_effective_char(table, ch);
    if (effective_ch == closer) {
      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
			"read: expected %s and value for hash before `%c'",
                        dot_name(params),
			ch);
	return NULL;
      }

      cdr = pair;
      if (!list)
	list = cdr;
      else
	SCHEME_CDR(last) = cdr;

      if (infixed) {
	/* Assert: we're not using the list stack */
	list = scheme_make_pair(infixed, list);
      }

      pop_indentation(indentation);
      list = attach_shape_tag(list, line, col, pos, SPAN(port, pos), stxsrc, params, closer);
      list = (stxsrc
	      ? scheme_make_stx_w_offset(list, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG)
	      : list);
      list = attach_shape_property(list, stxsrc, params, closer);
      return list;
    } else if (params->can_read_dot
	       && (effective_ch == '.')
	       && next_is_delim(port, params, brackets, braces)) {
      int dot_ch = ch;

      scheme_tell_all(port, &dotline, &dotcol, &dotpos);

      track_indentation(indentation, dotline, dotcol);

      if (((shape != mz_shape_cons) 
           && (shape != mz_shape_hash_elem)
           && (shape != mz_shape_vec_plus_infix))
          || infixed) {
	scheme_read_err(port, stxsrc, dotline, dotcol, dotpos, 1, 0, indentation,
			"read: illegal use of `%c'",
                        dot_ch);
	return NULL;
      }

      /* can't be eof, due to check above: */
      cdr = read_inner(port, stxsrc, ht, indentation, params, 0);
      ch = skip_whitespace_comments(port, stxsrc, ht, indentation, params, table, &prefetched);
      effective_ch = readtable_effective_char(table, ch);
      if ((effective_ch != closer) || (shape == mz_shape_vec_plus_infix)) {
	if (params->can_read_infix_dot 
            && (effective_ch == '.') 
            && next_is_delim(port, params, brackets, braces)) {
	  /* Parse as infix: */

	  if (shape == mz_shape_hash_elem) {
	    scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
			    "read: expected %s after hash value",
			    closer_name(params, closer));
	    return NULL;
	  }

	  {
	    scheme_tell_all(port, &dot2line, &dot2col, &dot2pos);
	    track_indentation(indentation, dot2line, dot2col);
	  }

	  infixed = cdr;

	  if (!list)
	    list = pair;
	  else
	    SCHEME_CDR(last) = pair;
	  last = pair;

	  /* Make sure there's not a closing paren immediately after the dot: */
	  ch = skip_whitespace_comments(port, stxsrc, ht, indentation, params, table, &prefetched);
          effective_ch = readtable_effective_char(table, ch);
	  if ((effective_ch == closer) || (ch == EOF)) {
	    scheme_read_err(port, stxsrc, dotline, dotcol, dotpos, 1, (ch == EOF) ? EOF : 0, indentation,
			    "read: illegal use of `%c'", ch);
	    return NULL;
	  }
          if (!prefetched)
            got_ch_already = 1;
	} else {
	  scheme_read_err(port, stxsrc, dotline, dotcol, dotpos, 1, (ch == EOF) ? EOF : 0, indentation,
			  "read: illegal use of `%c'",
                          dot_ch);
	  return NULL;
	}
      } else {
	SCHEME_CDR(pair) = cdr;
	cdr = pair;
	if (!list)
	  list = cdr;
	else
	  SCHEME_CDR(last) = cdr;

	/* Assert: infixed is NULL (otherwise we raised an exception above) */

	pop_indentation(indentation);
      list = attach_shape_tag(list, line, col, pos, SPAN(port, pos), stxsrc, params, closer);
        list = (stxsrc
		? scheme_make_stx_w_offset(list, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG)
		: list);
	list = attach_shape_property(list, stxsrc, params, closer);
	return list;
      }
    } else {
      if ((ch == SCHEME_SPECIAL)
          || (table
              && (ch != EOF)
              && (shape != mz_shape_hash_list)
              && (shape != mz_shape_fl_vec)
              && (shape != mz_shape_fx_vec))) {
	/* We have to try the read, because it might be a comment. */
	scheme_ungetc(ch, port);
	prefetched = read_inner(port, stxsrc, ht, indentation, params, 
                                RETURN_FOR_SPECIAL_COMMENT);
	if (!prefetched)
	  goto retry_before_dot;
        if ((shape == mz_shape_fl_vec) && !SCHEME_DBLP(prefetched)) {
          scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
                          "read: stream produced a non-flonum for flvector");
        } else if ((shape == mz_shape_fx_vec) && !SCHEME_INTP(prefetched)) {
          scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
                          "read: stream produced a non-fixnum for fxvector");
        }
      } else {
	got_ch_already = 1;
      }

      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, stxsrc, startline, startcol, start, SPAN(port, start), ch, indentation,
			"read: expected %s and value for hash",
                        dot_name(params));
	return NULL;
      }

      cdr = pair;
      if (!list)
	list = cdr;
      else
	SCHEME_CDR(last) = cdr;
      last = cdr;
    }
  }
}

static Scheme_Object *attach_shape_property(Scheme_Object *list, 
					    Scheme_Object *stxsrc, 
					    ReadParams *params, 
					    int closer)
{
  if ((closer != ')') && stxsrc) {
    Scheme_Object *opener;
    opener = ((closer == '}') 
	      ? scheme_paren_shape_preserve_curly
	      : scheme_paren_shape_preserve_square);
    return scheme_stx_property(list, scheme_paren_shape_symbol, opener);
  }
  return list;
}

static Scheme_Object *attach_shape_tag(Scheme_Object *list,
                        intptr_t line, intptr_t col, intptr_t pos, intptr_t span,
					    Scheme_Object *stxsrc, 
					    ReadParams *params, 
					    int closer)
{
  Scheme_Object *tag;
  tag = NULL;
  
  if (params->square_brackets_are_tagged && closer == ']') {
    tag = brackets_symbol;
  } else if (params->curly_braces_are_tagged && closer == '}') {
    tag = braces_symbol;
  }
  
  if (tag) {
    if (stxsrc) {
      tag = scheme_make_stx_w_offset(tag, line, col, pos, span, stxsrc, STX_SRCTAG);
    }
    list = scheme_make_pair(tag, list);
  }
  
  return list;
}

static Scheme_Object *read_flonum(Scheme_Object *port, 
				  Scheme_Object *stxsrc, 
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation, 
				  ReadParams *params,
				  int comment_mode)
{
  intptr_t line = 0, col = 0, pos = 0;
  intptr_t line2 = 0, col2 = 0, pos2 = 0;
  Scheme_Object *n;
  scheme_tell_all(port, &line, &col, &pos);
  n = read_number_literal(port, stxsrc, 1, 0, ht, indentation, params, comment_mode);
  if (SCHEME_DBLP(n))
    return n;
  else {
    scheme_tell_all(port, &line2, &col2, &pos2);
    scheme_read_err(port, stxsrc, line, col, pos, pos2-pos, -1, indentation, "read: expected flonum, got %V", n);
  }
  return NULL;
}

static Scheme_Object *read_fixnum(Scheme_Object *port, 
				  Scheme_Object *stxsrc, 
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation, 
				  ReadParams *params,
				  int comment_mode)
{
  intptr_t line = 0, col = 0, pos = 0;
  intptr_t line2 = 0, col2 = 0, pos2 = 0;
  Scheme_Object *n;
  scheme_tell_all(port, &line, &col, &pos);
  n = read_number_literal(port, stxsrc, 0, 1, ht, indentation, params, comment_mode);
  if (SCHEME_INTP(n))
    return n;
  else
    scheme_tell_all(port, &line2, &col2, &pos2);
    scheme_read_err(port, stxsrc, line, col, pos, pos2-pos, -1, indentation, "read: expected fixnum, got %V", n);
  return NULL;
}

static Scheme_Object *read_number_literal(Scheme_Object *port, 
                                          Scheme_Object *stxsrc, 
                                          int is_float, int is_not_float,
                                          Scheme_Hash_Table **ht,
                                          Scheme_Object *indentation, 
                                          ReadParams *params,
                                          int comment_mode)
{
  int ch;
  intptr_t line = 0, col = 0, pos = 0;
  Scheme_Object *special_value = NULL;
  Readtable *table;

  table = params->table;
  scheme_tell_all(port, &line, &col, &pos);
  ch = scheme_getc_special_ok(port);
  switch (ch) {
    case '+':
    case '-':
    case '.': /* ^^^ fallthrough ^^^ */
      special_value = read_plus_minus_period_leading_number(port, stxsrc, ch, line, col, pos, is_float, is_not_float, ht, indentation, params, table);
      break;
    case '#':
      ch = scheme_getc_special_ok(port);
      switch (ch ) {
	case 'X':
	case 'x': 
          return read_number(-1, port, stxsrc, line, col, pos, is_float, is_not_float, 16, 1, ht, indentation, params, table);
	  break;
	case 'B':
	case 'b': 
          return read_number(-1, port, stxsrc, line, col, pos, is_float, is_not_float, 2, 1, ht, indentation, params, table);
	  break;
	case 'O':
	case 'o': 
          return read_number(-1, port, stxsrc, line, col, pos, is_float, is_not_float, 8, 1, ht, indentation, params, table);
	  break;
	case 'D':
	case 'd': 
          return read_number(-1, port, stxsrc, line, col, pos, is_float, is_not_float, 10, 1, ht, indentation, params, table);
	  break;
        default:
          scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation, "read: expected `x', `X', `b', `B', `o', `O', `d', or `D'");
      }
    default:
      if (isdigit_ascii(ch))
	special_value = read_number(ch, port, stxsrc, line, col, pos, is_float, is_not_float, 10, 0, ht, indentation, params, table);
      else
        scheme_read_err(port, stxsrc, line, col, pos, 2, ch, indentation, "read: expected a digit, `+', `-', `.', or `#'");
  }
  return special_value;
}

/*========================================================================*/
/*                            string reader                               */
/*========================================================================*/

/* '"' has already been read */
static Scheme_Object *
read_string(int is_byte, Scheme_Object *port,
	    Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	    Scheme_Hash_Table **ht,
	    Scheme_Object *indentation, ReadParams *params, Readtable *table,
	    int err_ok)
{
  mzchar *buf, *oldbuf, onstack[32];
  int i, j, n, n1, ch, effective_ch, closer = '"';
  intptr_t size = 31, oldsize, in_pos, init_span;
  Scheme_Object *result;

  scheme_tell_all(port, NULL, NULL, &in_pos);
  init_span = in_pos - pos + 1;

  i = 0;
  buf = onstack;
  while (1) {
    ch = scheme_getc_special_ok(port);
    effective_ch = readtable_effective_char(table, ch);
    if (effective_ch == closer)
      break;

    if (ch == EOF) {
      if (err_ok)
	scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, init_span), ch, indentation,
			"read: expected a closing %s%s",
			"'\"'",
			(ch == EOF) ? "" : " after one character");
      return NULL;
    } else if (ch == SCHEME_SPECIAL) {
      scheme_get_ready_read_special(port, stxsrc, ht);
      if (err_ok)
	scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), SCHEME_SPECIAL, indentation,
			"read: found non-character while reading a %s",
			"string");
      return NULL;
    }
    /* Note: errors will tend to leave junk on the port, with an open \". */
    /* Escape-sequence handling by Eli Barzilay. */
    if (ch == '\\') {
      ch = scheme_getc_special_ok(port);
      if (ch == EOF) {
	if (err_ok)
	  scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, init_span), EOF, indentation,
			  "read: expected a closing %s",
			  "'\"'");
	return NULL;
      } else if (ch == SCHEME_SPECIAL) {
	scheme_get_ready_read_special(port, stxsrc, ht);
	if (err_ok)
	  scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), SCHEME_SPECIAL, indentation,
			  "read: found non-character while reading a %s",
			  "string");
	return NULL;
      }
      switch ( ch ) {
      case '\\': case '\"': case '\'': break;
      case 'a': ch = '\a'; break;
      case 'b': ch = '\b'; break;
      case 'e': ch = 27; break; /* escape */
      case 'f': ch = '\f'; break;
      case 'n': ch = '\n'; break;
      case 'r': ch = '\r'; break;
      case 't': ch = '\t'; break;
      case 'v': ch = '\v'; break;
      case '\r':
        if (scheme_peekc_special_ok(port) == '\n')
	  scheme_getc(port);
	continue; /* <---------- !!!! */
      case '\n':
        continue; /* <---------- !!!! */
      case 'x':
	ch = scheme_getc_special_ok(port);
	if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	  n = ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10);
	  ch = scheme_peekc_special_ok(port);
	  if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	    n = n*16 + (ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10));
	    scheme_getc(port); /* must be ch */
	  }
	  ch = n;
	} else {
	  if (ch == SCHEME_SPECIAL)
	    scheme_get_ready_read_special(port, stxsrc, ht);
	  if (err_ok)
	    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
			    "read: no hex digit following \\x in %s",
			    "string");
	  return NULL;
	}
	break;
      case 'u':
      case 'U':
	if (!is_byte) {
	  int maxc = ((ch == 'u') ? 4 : 8);
          char initial[9];
	  ch = scheme_getc_special_ok(port);
	  if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	    int count = 1;
            initial[0] = ch;
	    n = ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10);
	    while (count < maxc) {
	      ch = scheme_peekc_special_ok(port);
	      if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                initial[count] = ch;
		n = ((unsigned)n<<4) + (ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10));
		scheme_getc(port); /* must be ch */
		count++;
	      } else
		break;
	    }
            initial[count] = 0;
            if ((maxc == 4) && ((n >= 0xD800) && (n <= 0xDBFF))) {
              /* Allow a surrogate-pair-like encoding, as long as
                 the next part is "\uD..." */
              int n2 = -1, sndp = 0;
              mzchar snd[7];
              ch = scheme_getc_special_ok(port);
              if (ch == '\\') {
                snd[sndp++] = ch;
                ch = scheme_getc_special_ok(port);
                if (ch == 'u') {
                  snd[sndp++] = ch;
                  ch = scheme_getc_special_ok(port);
                  if ((ch == 'd') || (ch == 'D')) {
                    snd[sndp++] = ch;
                    ch = scheme_getc_special_ok(port);
                    if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                      snd[sndp++] = ch;
                      n2 = (scheme_toupper(ch)-'A'+10);
                      if ((n2 >= 12) && (n2 <= 15)) {
                        n2 = 0xD000 | (n2 << 8);
                        ch = scheme_getc_special_ok(port);
                        if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                          snd[sndp++] = ch;
                          n2 |= ((ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10)) << 4);
                          ch = scheme_getc_special_ok(port);
                          if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                            n2 |= (ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10));
                            n = (((n - 0xD800) << 10) + (n2 - 0xDC00)) + 0x10000;
                          } else
                            n2 = -1;
                        } else
                          n2 = -1;
                      } else
                        n2 = -1;
                    }
                  }
                }
              }
              if (n2 < 0) {
                if (ch == SCHEME_SPECIAL)
                  scheme_get_ready_read_special(port, stxsrc, ht);
                else if (NOT_EOF_OR_SPECIAL(ch))
                  snd[sndp++] = ch;
                snd[sndp] = 0;
                if (err_ok)
                  scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
                                  "read: bad or incomplete surrogate-style encoding at `\\u%s%5'",
                                  initial,
                                  snd);
                return NULL;
              }
            }
	    /* disallow surrogate points, etc */
	    if (((n >= 0xD800) && (n <= 0xDFFF))
		|| (n > 0x10FFFF)) {
	      ch = -1;
	    } else {
	      ch = n;
	    }
	  } else {
	    if (ch == SCHEME_SPECIAL)
	      scheme_get_ready_read_special(port, stxsrc, ht);
	    if (err_ok)
	      scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
			      "read: no hex digit following \\%c in %s",
			      ((maxc == 4) ? 'u' : 'U'),
			      "string");
	    return NULL;
	  }
	  break;
	} /* else FALLTHROUGH!!! */
      default:
	if ((ch >= '0') && (ch <= '7')) {
	  for (n = j = 0; j < 3; j++) {
	    n1 = 8*n + ch - '0';
	    if (n1 > 255) {
	      if (err_ok)
		scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
				"read: escape sequence \\%o out of range in %s", n1,
				"string");
	      return NULL;
	    }
	    n = n1;
	    if (j < 2) {
	      ch = scheme_peekc_special_ok(port);
	      if (!((ch >= '0') && (ch <= '7'))) {
		break;
	      } else {
		scheme_getc(port); /* must be ch */
	      }
	    }
	  }
	  ch = n;
	} else {
	  if (err_ok)
	    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
			    "read: unknown escape sequence \\%c in %s%s", ch,
			    is_byte ? "byte " : "",
			    "string");
	  return NULL;
	}
	break;
      }
    } else if ((ch == '\n') || (ch == '\r')) {
      /* Suspicious string... remember the line */
      if (line > 0) {
	if (SCHEME_PAIRP(indentation)) {
	  Scheme_Indent *indt;
	  indt = (Scheme_Indent *)SCHEME_CAR(indentation);
	  /* Only remember if there's no earlier suspcious string line: */
	  if (!indt->suspicious_quote) {
	    indt->suspicious_quote = line;
	  }
	}
      }
    } else if (is_byte && (ch > 255)) {
      if (err_ok)
	scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
			"read: out-of-range character in byte string: %c",
                        ch);
      return NULL;
    }

    if (ch < 0) {
      if (err_ok)
	scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
			"read: out-of-range character in %sstring",
			is_byte ? "byte " : "");
      return NULL;
    }

    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
      memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
    }
    buf[i++] = ch;
  }
  buf[i] = '\0';

  if (!is_byte)
    result = scheme_make_immutable_sized_char_string(buf, i, i <= 31);
  else {
    /* buf is not UTF-8 encoded; all of the chars are less than 256.
       We just need to change to bytes.. */
    char *s;
    s = (char *)scheme_malloc_atomic(i + 1);
    for (j = 0; j < i; j++) {
      ((unsigned char *)s)[j] = buf[j];
    }
    s[i] = 0;
    result = scheme_make_immutable_sized_byte_string(s, i, 0);
  }
  if (stxsrc) {
    result = scheme_intern_literal_string(result);
    result =  scheme_make_stx_w_offset(result, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
  }

  return result;
}

Scheme_Object *scheme_read_byte_string(Scheme_Object *port)
/* used by GRacket */
{
  return read_string(1, port,
		     NULL, 0, 0, 0,
		     NULL,
		     NULL, NULL, NULL,
		     0);
}

static Scheme_Object *
read_here_string(Scheme_Object *port, Scheme_Object *stxsrc,
		 intptr_t line, intptr_t col, intptr_t pos,
		 Scheme_Object *indentation,
		 ReadParams *params)
     /* #<< has been read already */
{
  int tlen = 0, len = 0, size = 12;
  mzchar *tag, *naya, *s, buf[12], c;
  intptr_t in_pos, init_span;
  Scheme_Object *str;

  scheme_tell_all(port, NULL, NULL, &in_pos);
  init_span = in_pos - pos + 1;

  tag = buf;
  while (1) {
    c = scheme_getc(port);
    if (c == '\n') {
      break;
    } else if (c == EOF) {
      scheme_read_err(port, stxsrc, line, col, pos, 3 + tlen, EOF, indentation,
		      "read: found end-of-file after #<< and before first and-of-line");
      return NULL;
    } else {
      if (tlen >= size) {
	size *= 2;
	naya = (mzchar *)scheme_malloc_atomic(size * sizeof(mzchar));
	memcpy(naya, tag, tlen * sizeof(mzchar));
	tag = naya;
      }
      tag[tlen++] = c;
    }
  }
  if (!tlen) {
    scheme_read_err(port, stxsrc, line, col, pos, 3, 0, indentation,
		    "read: no characters after #<< before and-of-line");
    return NULL;
  }

  size = 10 + tlen;
  s = (mzchar *)scheme_malloc_atomic(size * sizeof(mzchar));
  while (1) {
    c = scheme_getc(port);
    if (c == EOF) {
      scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, init_span), EOF, indentation,
		      "read: found end-of-file before terminating %u%s",
		      tag, 
		      (intptr_t)((tlen > 50) ? 50 : tlen),
		      (tlen > 50) ? "..." : "");
      return NULL;
    }
    if (len >= size) {
      size *= 2;
      naya = (mzchar *)scheme_malloc_atomic(size * sizeof(mzchar));
      memcpy(naya, s, len * sizeof(mzchar));
      s = naya;
    }
    s[len++] = c;
    if ((len >= tlen)
	&& ((len == tlen)
	    || (s[len - tlen - 1] == '\n'))
	&& !memcmp(s XFORM_OK_PLUS (len - tlen), tag, sizeof(mzchar) * tlen)) {
      c = scheme_peekc(port);
      if ((c == '\r') || (c == '\n') || (c == EOF))
	break;
    }
  }

  len -= (tlen + 1);
  if (len < 0)
    len = 0;

  str = scheme_make_immutable_sized_char_string(s, len, 1);

  if (stxsrc) {
    str = scheme_intern_literal_string(str);
    str = scheme_make_stx_w_offset(str, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
  }

  return str;
}

char *scheme_extract_indentation_suggestions(Scheme_Object *indentation)
{
  intptr_t suspicious_quote = 0;
  char *suspicions = "";

  /* search back through indentation records to find the
     first suspicious quote */
  while (SCHEME_PAIRP(indentation)) {
    Scheme_Indent *indt;
    indt = (Scheme_Indent *)SCHEME_CAR(indentation);
    indentation = SCHEME_CDR(indentation);
    if (indt->suspicious_quote) {
      suspicious_quote = indt->suspicious_quote;
    }
  }

  if (suspicious_quote) {
    suspicions = (char *)scheme_malloc_atomic(64);
    sprintf(suspicions,
	    "newline within %s suggests a missing %s on line %" PRIdPTR,
	    "string",
	    "'\"'",
	    suspicious_quote);
  }

  return suspicions;
}

/*========================================================================*/
/*                            vector reader                               */
/*========================================================================*/
#define FUNC_NAME read_vector
#define VTYPE_STR "vector"
#define VEC_TYPE Scheme_Object
#define ELMS_TYPE Scheme_Object **
#define ELM_TYPE Scheme_Object *
#define MZ_SHAPE allow_infix ? mz_shape_vec_plus_infix : mz_shape_vec
#define MK_VEC() (Scheme_Object *) scheme_make_vector(requestLength, NULL)
#define ELMS_SELECTOR SCHEME_VEC_ELS
#define ELM_SELECTOR
#define ELM_MAKE_ZERO scheme_make_integer(0)
#define ELM_STX(elm) scheme_make_stx_w_offset(elm, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
#define VEC_SIZE SCHEME_VEC_SIZE
#include "read_vector.inc"

#define FUNC_NAME read_fxvector
#define VTYPE_STR "fxvector"
#define VEC_TYPE Scheme_Object
#define ELMS_TYPE Scheme_Object **
#define ELM_TYPE Scheme_Object *
#define MZ_SHAPE mz_shape_fx_vec
#define MK_VEC() (Scheme_Object *) scheme_alloc_fxvector(requestLength)
#define ELMS_SELECTOR SCHEME_FXVEC_ELS
#define ELM_SELECTOR
#define ELM_MAKE_ZERO scheme_make_integer(0)
#define ELM_STX(elm) elm
#define VEC_SIZE SCHEME_FXVEC_SIZE
#include "read_vector.inc"

#define FUNC_NAME read_flvector
#define VTYPE_STR "flvector"
#define VEC_TYPE Scheme_Double_Vector
#define ELMS_TYPE double *
#define ELM_TYPE double
#define MZ_SHAPE mz_shape_fl_vec
#define MK_VEC() scheme_alloc_flvector(requestLength)
#define ELMS_SELECTOR SCHEME_FLVEC_ELS
#define ELM_SELECTOR SCHEME_DBL_VAL
#define ELM_MAKE_ZERO 0.0
#define ELM_STX(elm) elm
#define VEC_SIZE SCHEME_FLVEC_SIZE
#include "read_vector.inc"

/*========================================================================*/
/*                            symbol reader                               */
/*========================================================================*/

/* Also dispatches to number reader, since things not-a-number are
   symbols. */

typedef int (*Getc_Fun_r)(Scheme_Object *port);

/* nothing has been read, except maybe some flags */
static Scheme_Object  *
read_number_or_symbol(int init_ch, int skip_rt, Scheme_Object *port,
		      Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
		      int is_float, int is_not_float,
		      int radix, int radix_set,
		      int is_symbol, int is_kw, int pipe_quote,
		      Scheme_Hash_Table **ht,
		      Scheme_Object *indentation, ReadParams *params, Readtable *table)
{
  mzchar *buf, *oldbuf, onstack[MAX_QUICK_SYMBOL_SIZE];
  int size, oldsize;
  int i, ch, quoted, quoted_ever = 0, running_quote = 0;
  int running_quote_ch = 0;
  intptr_t rq_pos = 0, rq_col = 0, rq_line = 0;
  int case_sens = params->case_sensitive;
  int decimal_inexact = params->read_decimal_inexact;
  int read_cdot = params->read_cdot;
  Scheme_Object *o;
  int delim_ok;
  int ungetc_ok;
  int far_char_ok;
  int single_escape, multiple_escape, norm_count = 0;
  Getc_Fun_r getc_special_ok_fun;

  if (!skip_rt && table) {
    /* If the readtable provides a "symbol" reader, then use it: */
    if (table->symbol_parser) {
      return readtable_call(1, init_ch, table->symbol_parser, params,
			    port, stxsrc, line, col, pos, 0, ht, NULL);
      /* Special-comment result is handled in main loop. */
    }
  }

  ungetc_ok = scheme_peekc_is_ungetc(port);

  if (ungetc_ok) {
    getc_special_ok_fun = scheme_getc_special_ok;
  } else {
    getc_special_ok_fun = scheme_peekc_special_ok;
  }

  i = 0;
  size = MAX_QUICK_SYMBOL_SIZE - 1;
  buf = onstack;

  if (init_ch < 0)
    ch = getc_special_ok_fun(port);
  else {
    /* Assert: this one won't need to be ungotten */
    ch = init_ch;
  }

  if (table) {
    far_char_ok = 0;
    delim_ok = 0;
  } else {
    delim_ok = SCHEME_OK;
    far_char_ok = 1;
  }

  while (NOT_EOF_OR_SPECIAL(ch)
	 && (running_quote
	     || (!table 
		 && !scheme_isspace(ch) 
		 && (((ch < 128) && (delim[ch] & delim_ok))
		     || ((ch >= 128) && far_char_ok))
         && !(!is_float && !is_not_float && !radix_set && read_cdot && ch == '.'))
	     || (table
         && !(!is_float && !is_not_float && !radix_set && read_cdot && readtable_effective_char(table, ch) == '.')))) {
    if (table) {
      int v;
      v = readtable_kind(table, ch, params);
      if (!running_quote && (v & (READTABLE_TERMINATING | READTABLE_WHITESPACE)))
	break;
      single_escape = (v & READTABLE_SINGLE_ESCAPE);
      multiple_escape = (v & READTABLE_MULTIPLE_ESCAPE);
    } else {
      single_escape = (ch == '\\');
      multiple_escape = ((ch == '|') && pipe_quote);
    }
    if (!ungetc_ok) {
      if (init_ch < 0)
	scheme_getc(port); /* must be a character */
      else
	init_ch = -1;
    }
    if (single_escape && !running_quote) {
      int esc_ch = ch;
      ch = scheme_getc_special_ok(port);
      if (ch == EOF) {
	scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), EOF, indentation,
			"read: EOF following `%c' in %s", esc_ch, is_kw ? "keyword" : "symbol");
	return NULL;
      } else if (ch == SCHEME_SPECIAL) {
	scheme_get_ready_read_special(port, stxsrc, ht);
	scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), SCHEME_SPECIAL, indentation,
			"read: non-character following `%c' in %s", esc_ch, is_kw ? "keyword" : "symbol");
	return NULL;
      }
      quoted = 1;
      quoted_ever = 1;
    } else if (multiple_escape && (!running_quote || (ch == running_quote_ch))) {
      quoted_ever = 1;
      running_quote = !running_quote;
      running_quote_ch = ch;
      quoted = 0;

      scheme_tell_all(port, &rq_line, &rq_col, &rq_pos);

      ch = getc_special_ok_fun(port);
      continue; /* <-- !!! */
    } else
      quoted = 0;

    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
      memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
    }

    if (!case_sens && !quoted && !running_quote)
      norm_count++;
    else if (norm_count) {
      /* case-normalize the last norm_count characters */
      mzchar *s;
      int newlen;
      s = scheme_string_recase(buf, i - norm_count, norm_count, 3, 1, &newlen);
      if (s != buf) {
	if ((i + newlen - norm_count) >= size) {
	  oldsize = size;
	  oldbuf = buf;
	  
	  size *= 2;
	  if (size <= (i + newlen - norm_count))
	    size = 2 * (i + (newlen - norm_count));
	  buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
	  memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
	}
	memcpy(buf + i - norm_count, s, sizeof(mzchar) * newlen);
      }
      i += (newlen - norm_count);
      norm_count = 0;
    }

    buf[i++] = ch;

    ch = getc_special_ok_fun(port);
  }

  if (running_quote && (ch == SCHEME_SPECIAL)) {
    scheme_get_ready_read_special(port, stxsrc, ht);
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), SCHEME_SPECIAL, indentation,
		    "read: non-character following `%c' in %s", running_quote_ch,
		    is_kw ? "keyword" : "symbol");
  }

  if (ungetc_ok)
    scheme_ungetc(ch, port);

  if (running_quote) {
    scheme_read_err(port, stxsrc, rq_line, rq_col, rq_pos, SPAN(port, rq_pos), EOF, indentation,
		    "read: unbalanced `%c'", running_quote_ch);
    return NULL;
  }

  if (norm_count) {
    /* case-normalize the last norm_count characters */
    mzchar *s;
    int newlen;
    s = scheme_string_recase(buf, i - norm_count, norm_count, 3, 1, &newlen);
    if (s != buf) {
      oldsize = size;
      oldbuf = buf;
      size = i + (newlen - norm_count) + 1;
      buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
      memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
      memcpy(buf + i - norm_count, s, sizeof(mzchar) * newlen);
    }
    i += (newlen - norm_count);
  }

  buf[i] = '\0';

  if (!quoted_ever && (i == 1) 
      && (readtable_effective_char(params->table, buf[0]) == '.')) {
    intptr_t xl, xc, xp;
    scheme_tell_all(port, &xl, &xc, &xp);
    scheme_read_err(port, stxsrc, xl, xc, xp,
		    1, 0, indentation,
		    "read: illegal use of `.'");
    return NULL;
  }

  if ((is_symbol || quoted_ever) && !is_float && !is_not_float && !radix_set)
    o = scheme_false;
  else {
    o = scheme_read_number(buf, i,
			   is_float, is_not_float, decimal_inexact,
			   radix, radix_set,
			   port, NULL, 0,
			   stxsrc, line, col, pos, SPAN(port, pos),
			   indentation);
    if (!SCHEME_INTP(o) && stxsrc)
      o = scheme_intern_literal_number(o);
  }

  if (SAME_OBJ(o, scheme_false)) {
    if (is_kw)
      o = scheme_intern_exact_char_keyword(buf, i);
    else
      o = scheme_intern_exact_char_symbol(buf, i);
  }

  if (stxsrc)
    o = scheme_make_stx_w_offset(o, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);

  return o;
}

static Scheme_Object  *
read_number(int init_ch,
	    Scheme_Object *port,
	    Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	    int is_float, int is_not_float,
	    int radix, int radix_set,
	    Scheme_Hash_Table **ht,
	    Scheme_Object *indentation, ReadParams *params, Readtable *table)
{
  return read_number_or_symbol(init_ch, init_ch < 0,
			       port, stxsrc, line, col, pos,
			       is_float, is_not_float,
			       radix, radix_set, 0, 0,
			       params->can_read_pipe_quote,
			       ht, indentation, params, table);
}

static Scheme_Object  *
read_symbol(int init_ch,
	    int skip_rt,
	    Scheme_Object *port,
	    Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	    Scheme_Hash_Table **ht,
	    Scheme_Object *indentation, ReadParams *params, Readtable *table)
{
  return read_number_or_symbol(init_ch, skip_rt,
			       port, stxsrc, line, col, pos,
			       0, 0, 10, 0, 1, 0,
			       params->can_read_pipe_quote,
			       ht, indentation, params, table);
}

static Scheme_Object  *
read_keyword(int init_ch,
	     Scheme_Object *port,
	     Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	     Scheme_Hash_Table **ht,
	     Scheme_Object *indentation, ReadParams *params, Readtable *table)
{
  return read_number_or_symbol(init_ch, 1,
			       port, stxsrc, line, col, pos,
			       0, 0, 10, 0, 1, 1,
			       params->can_read_pipe_quote,
			       ht, indentation, params, table);
}

static Scheme_Object  *
read_delimited_constant(int ch, const mzchar *str,
                        Scheme_Object *v,
                        Scheme_Object *port,
                        Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
                        Scheme_Object *indentation,
                        ReadParams *params, Readtable *table)
{
  int first_ch = ch;
  int scanpos = 1;

  if (ch == str[0]) { /* might be `T' instead of `t', for example */
    do {
      ch = scheme_getc_special_ok(port);
      if ((mzchar)ch == str[scanpos]) {
        scanpos++;
      } else {
        break;
      }
    } while (str[scanpos]);
  } else {
    /* need to show next character to show why it's wrong: */
    ch = scheme_getc_special_ok(port);
  }

  if (str[scanpos]
      || !next_is_delim(port, params, 1, 1)) {
    mzchar str_part[7], one_more[2];

    if (!str[scanpos]) {
      /* get non-delimiter again: */
      ch = scheme_getc_special_ok(port);
    }

    memcpy(str_part, str XFORM_OK_PLUS 1, (scanpos - 1) * sizeof(mzchar));
    str_part[scanpos - 1] = 0;
    if (NOT_EOF_OR_SPECIAL(ch)) {
      one_more[0] = ch;
      one_more[1] = 0;
    } else
      one_more[0] = 0;

    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos),
                    ch, indentation,
                    "read: bad syntax `#%c%5%u'",
                    first_ch,
                    str_part,
                    one_more, 
                    (intptr_t)(NOT_EOF_OR_SPECIAL(ch) ? 1 : 0));
    return NULL;
  }
            
  return (stxsrc
          ? scheme_make_stx_w_offset(v, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG)
          : v);
}

/*========================================================================*/
/*                              char reader                               */
/*========================================================================*/

static int u_strcmp(mzchar *s, const char *_t)
{
  int i;
  unsigned char *t = (unsigned char *)_t;

  for (i = 0; s[i] && (scheme_tolower(s[i]) == scheme_tolower((mzchar)((unsigned char *)t)[i])); i++) {
  }
  if (s[i] || t[i])
    return 1;
  return 0;
}

static Scheme_Object *make_interned_char(int ch, Scheme_Object *stxsrc)
{
  if (ch < 256)
    return scheme_make_character(ch);
  else if (stxsrc)
    return scheme_intern_literal_number(scheme_make_char(ch));
  else
    return scheme_make_char(ch);
}

/* "#\" has been read */
static Scheme_Object *
read_character(Scheme_Object *port,
	       Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	       Scheme_Hash_Table **ht,
	       Scheme_Object *indentation, ReadParams *params)
{
  int ch, next;

  ch = scheme_getc_special_ok(port);

  if (ch == SCHEME_SPECIAL) {
    scheme_get_ready_read_special(port, stxsrc, ht);
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), SCHEME_SPECIAL, indentation,
		    "read: found non-character after #\\");
    return NULL;
  }

  next = scheme_peekc_special_ok(port);

  if ((ch >= '0' && ch <= '7') && (next >= '0' && next <= '7')) {
    /* a is the same as next */
    int last;

    last = (scheme_getc(port) /* is char */, scheme_peekc_special_ok(port));

    if (last != SCHEME_SPECIAL)
      scheme_getc(port); /* must be last */

    if (last < '0' || last > '7' || ch > '3') {
      scheme_read_err(port, stxsrc, line, col, pos, ((last == EOF) || (last == SCHEME_SPECIAL)) ? 3 : 4, last, indentation,
		      "read: bad character constant #\\%c%c%c",
		      ch, next, ((last == EOF) || (last == SCHEME_SPECIAL)) ? ' ' : last);
      return NULL;
    }

    ch = ((ch - '0') << 6) + ((next - '0') << 3) + (last - '0');

    return make_interned_char(ch, stxsrc);
  }

  if (((ch == 'u') || (ch == 'U')) && NOT_EOF_OR_SPECIAL(next) && scheme_isxdigit(next)) {
    int count = 0, n = 0, nbuf[10], maxc = ((ch == 'u') ? 4 : 8);
    while (count < maxc) {
      ch = scheme_peekc_special_ok(port);
      if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	nbuf[count] = ch;
	n = ((unsigned)n<<4) + (ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10));
	scheme_getc(port); /* must be ch */
	count++;
      } else
	break;
    }
    /* disallow surrogate points, etc. */
    if ((n < 0)
	|| ((n >= 0xD800) && (n <= 0xDFFF))
	|| (n > 0x10FFFF)) {
      scheme_read_err(port, stxsrc, line, col, pos, count + 2, 0, indentation,
		      "read: bad character constant #\\%c%u",
		      (maxc == 4) ? 'u' : 'U',
		      nbuf, (intptr_t)count);
      return NULL;
    } else {
      ch = n;
    }
  } else if ((ch != EOF) && scheme_isalpha(ch) && NOT_EOF_OR_SPECIAL(next) && scheme_isalpha(next)) {
    mzchar *buf, *oldbuf, onstack[32];
    int i;
    intptr_t size = 31, oldsize;

    i = 1;
    buf = onstack;
    buf[0] = ch;
    while ((ch = scheme_peekc_special_ok(port), NOT_EOF_OR_SPECIAL(ch) && scheme_isalpha(ch))) {
      scheme_getc(port); /* is alpha character */
      if (i >= size) {
	oldsize = size;
	oldbuf = buf;

	size *= 2;
	buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
	memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
      }
      buf[i++] = ch;
    }
    buf[i] = '\0';

    switch (scheme_tolower(buf[0])) {
    case 'n': /* maybe `newline' or 'null' or 'nul' */
      if (!u_strcmp(buf, "newline"))
	return scheme_make_char('\n');
      if (!u_strcmp(buf, "null") || !u_strcmp(buf, "nul"))
	return scheme_make_char('\0');
      break;
    case 's': /* maybe `space' */
      if (!u_strcmp(buf, "space"))
	return scheme_make_char(' ');
      break;
    case 'r': /* maybe `rubout' or `return' */
      if (!u_strcmp(buf, "rubout"))
	return scheme_make_char(0x7f);
      if (!u_strcmp(buf, "return"))
	return scheme_make_char('\r');
      break;
    case 'p': /* maybe `page' */
      if (!u_strcmp(buf, "page"))
	return scheme_make_char('\f');
      break;
    case 't': /* maybe `tab' */
      if (!u_strcmp(buf, "tab"))
	return scheme_make_char('\t');
      break;
    case 'v': /* maybe `vtab' */
      if (!u_strcmp(buf, "vtab"))
	return scheme_make_char(0xb);
      break;
    case 'b': /* maybe `backspace' */
      if (!u_strcmp(buf, "backspace"))
	return scheme_make_char('\b');
      break;
    case 'l': /* maybe `linefeed' */
      if (!u_strcmp(buf, "linefeed"))
	return scheme_make_char('\n');
      break;
    default:
      break;
    }

    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), 0, indentation,
		    "read: bad character constant: #\\%5",
		    buf);
  }

  if (ch == EOF) {
    scheme_read_err(port, stxsrc, line, col, pos, 2, EOF, indentation,
		    "read: expected a character after #\\");
  }

  return make_interned_char(ch, stxsrc);
}

/*========================================================================*/
/*                            quote readers                               */
/*========================================================================*/

/* "'", etc. has been read */
static Scheme_Object *
read_quote(char *who, Scheme_Object *quote_symbol, int len,
	   Scheme_Object *port,
	   Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
	   Scheme_Hash_Table **ht,
	   Scheme_Object *indentation, ReadParams *params)
{
  Scheme_Object *obj, *ret;

  obj = read_inner(port, stxsrc, ht, indentation, params, 0);
  if (SCHEME_EOFP(obj))
    scheme_read_err(port, stxsrc, line, col, pos, len, EOF, indentation,
		    "read: expected an element for %s (found end-of-file)",
		    who);
  ret = (stxsrc
	 ? scheme_make_stx_w_offset(quote_symbol, line, col, pos, len, stxsrc, STX_SRCTAG)
	 : quote_symbol);
  ret = scheme_make_pair(ret, scheme_make_pair(obj, scheme_null));
  if (stxsrc) {
    ret = scheme_make_stx_w_offset(ret, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
  }
  return ret;
}

/* "#&" has been read */
static Scheme_Object *read_box(Scheme_Object *port,
			       Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
			       Scheme_Hash_Table **ht,
			       Scheme_Object *indentation, ReadParams *params)
{
  Scheme_Object *o, *bx;

  o = read_inner(port, stxsrc, ht, indentation, params, 0);

  if (SCHEME_EOFP(o))
    scheme_read_err(port, stxsrc, line, col, pos, 2, EOF, indentation,
		    "read: expected an element for #& box (found end-of-file)");

  bx = scheme_box(o);

  if (stxsrc) {
    SCHEME_SET_BOX_IMMUTABLE(bx);
    bx = scheme_make_stx_w_offset(bx, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
  }

  return bx;
}

/*========================================================================*/
/*                         hash table reader                              */
/*========================================================================*/

/* "(" has been read */
static Scheme_Object *read_hash(Scheme_Object *port, Scheme_Object *stxsrc,
				intptr_t line, intptr_t col, intptr_t pos,
				int opener, char closer,  int kind,
				Scheme_Hash_Table **ht,
				Scheme_Object *indentation,
                                ReadParams *params, Readtable *table)
{
  Scheme_Object *l;

  /* using mz_shape_hash_list ensures that l is a list of pairs */
  l = read_list(port, stxsrc, line, col, pos, opener, closer, mz_shape_hash_list, 0, ht, indentation, params, table);

  if (stxsrc) {
    Scheme_Object *key, *val;
    Scheme_Hash_Tree *t;

    t = scheme_make_hash_tree(kind);

    for (; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
      val = SCHEME_STX_CAR(l);
      key = SCHEME_STX_CAR(val);
      key = scheme_syntax_to_datum(key, 0, NULL);
      val = SCHEME_STX_CDR(val);
      
      t = scheme_hash_tree_set(t, key, val);
    }
    
    return scheme_make_stx_w_offset((Scheme_Object *)t, line, col, pos, SPAN(port, pos), stxsrc, STX_SRCTAG);
  } else {
    /* Wait for placeholders to be resolved before mapping keys to
       values, because a placeholder may be used in a key. */
    Scheme_Object *ph;

    ph = scheme_alloc_object();
    ph->type = scheme_table_placeholder_type;
    SCHEME_IPTR_VAL(ph) = l;
    SCHEME_PINT_VAL(ph) = kind;

    if (!*ht) {
      /* So that resolve_references is called to build the table: */
      Scheme_Hash_Table *tht;
      tht = scheme_make_hash_table(SCHEME_hash_ptr);
      *ht = tht;
    }

    return ph;
  }
}

/*========================================================================*/
/*                               intern                                   */
/*========================================================================*/

static Scheme_Object *read_intern(int argc, Scheme_Object **argv)
{
  return scheme_read_intern(argv[0]);
}

Scheme_Object *scheme_read_intern(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && SCHEME_NUMBERP(o))
    o = scheme_intern_literal_number(o);
  else if (SCHEME_CHAR_STRINGP(o)) {
    if (!SCHEME_IMMUTABLEP(o))
      o = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(o),
                                                  SCHEME_CHAR_STRLEN_VAL(o),
                                                  1);
    o = scheme_intern_literal_string(o);
  } else if (SCHEME_BYTE_STRINGP(o)) {
    if (!SCHEME_IMMUTABLEP(o))
      o = scheme_make_immutable_sized_byte_string(SCHEME_BYTE_STR_VAL(o),
                                                  SCHEME_BYTE_STRLEN_VAL(o),
                                                  1);
    o = scheme_intern_literal_string(o);
  } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_regexp_type))
    o = scheme_intern_literal_string(o);
  else if (SCHEME_CHARP(o) && (SCHEME_CHAR_VAL(o) >= 256))
    o = scheme_intern_literal_number(o);

  return o;
}

/*========================================================================*/
/*                               utilities                                */
/*========================================================================*/

static int
skip_whitespace_comments(Scheme_Object *port, Scheme_Object *stxsrc,
			 Scheme_Hash_Table **ht, Scheme_Object *indentation,
                         ReadParams *params, Readtable *table,
                         Scheme_Object **_prefetched)
/* If `_prefetched` is non_NULL, then a SCHEME_SPECIAL result means that
   the special value has already been read, and it wasn't a comment. */
{
  int ch, effective_ch;
  int blockc_1, blockc_2;

  blockc_1 = '#';
  blockc_2 = '|';

 start_over:

  if (table) {
    while ((ch = scheme_getc_special_ok(port), NOT_EOF_OR_SPECIAL(ch))) {
      if (!(readtable_kind(table, ch, params) & READTABLE_WHITESPACE))
	break;
    }
  } else {
    while ((ch = scheme_getc_special_ok(port), NOT_EOF_OR_SPECIAL(ch) && scheme_isspace(ch))) {}
  }

  effective_ch = readtable_effective_char(table, ch);
  if (effective_ch == ';') {
    do {
      ch = scheme_getc_special_ok(port);
      effective_ch = readtable_effective_char(table, ch);
      if (effective_ch == SCHEME_SPECIAL)
	scheme_get_ready_read_special(port, stxsrc, ht);
    } while (!is_line_comment_end(effective_ch) && (effective_ch != EOF));
    goto start_over;
  }

  if ((effective_ch == blockc_1)
      && (readtable_effective_char(table, scheme_peekc_special_ok(port)) == blockc_2)) {
    int depth = 0;
    int ch2 = 0;
    intptr_t col, pos, line;

    scheme_tell_all(port, &line, &col, &pos);

    (void)scheme_getc(port); /* re-read '|' */
    do {
      ch = scheme_getc_special_ok(port);
      effective_ch = readtable_effective_char(table, ch);
 
      if (effective_ch == EOF)
	scheme_read_err(port, stxsrc, line, col, pos, MINSPAN(port, pos, 2), EOF, indentation,
			"read: end of file in #| comment");
      else if (effective_ch == SCHEME_SPECIAL)
	scheme_get_ready_read_special(port, stxsrc, ht);

      if ((ch2 == blockc_2) && (effective_ch == blockc_1)) {
	if (!(depth--))
	  goto start_over;
	effective_ch = 0; /* So we don't count '#' toward an opening "#|" */
      } else if ((ch2 == blockc_1) && (ch == blockc_2)) {
	depth++;
	effective_ch = 0; /* So we don't count '|' toward a closing "|#" */
      }
      ch2 = effective_ch;
    } while (1);

    goto start_over;
  }
  if ((effective_ch == '#')
      && (readtable_effective_char(table, scheme_peekc_special_ok(port)) == ';')) {
    Scheme_Object *skipped;
    intptr_t col, pos, line;

    scheme_tell_all(port, &line, &col, &pos);

    track_indentation(indentation, line, col);

    (void)scheme_getc(port); /* re-read ';' */

    skipped = read_inner(port, stxsrc, ht, indentation, params, 0);
    if (SCHEME_EOFP(skipped))
      scheme_read_err(port, stxsrc, line, col, pos,  MINSPAN(port, pos, 2), EOF, indentation,
		      "read: expected a commented-out element for `#;' (found end-of-file)");

    /* For resolving graphs introduced in #; : */
    if (*ht) {
      Scheme_Object *v;
      v = scheme_hash_get(*ht, unresolved_uninterned_symbol);
      if (!v)
	v = scheme_null;
      v = scheme_make_pair(skipped, v);
      scheme_hash_set(*ht, unresolved_uninterned_symbol, v);
    }

    goto start_over;
  }

  if ((ch == SCHEME_SPECIAL) && _prefetched) {
    Scheme_Object *v;
    intptr_t col, pos, line;

    scheme_tell_all(port, &line, &col, &pos);
    v = scheme_get_special(port, stxsrc, line, col, pos, 0, ht);
    if (!scheme_special_comment_value(v)) {
      *_prefetched = v;
      return SCHEME_SPECIAL;
    }

    goto start_over;
  }

  return ch;
}

static void unexpected_closer(int ch,
			      Scheme_Object *port, Scheme_Object *stxsrc,
			      intptr_t line, intptr_t col, intptr_t pos,
			      Scheme_Object *indentation,
                              ReadParams *params)
{
  char *suggestion = "", *found = "unexpected";

  if (SCHEME_PAIRP(indentation)) {
    Scheme_Indent *indt;
    int opener;
    char *missing;

    indt = (Scheme_Indent *)SCHEME_CAR(indentation);

    found = scheme_malloc_atomic(100);

    if (indt->closer == '}')
      opener = '{';
    else if (indt->closer == ']')
      opener = '[';
    else
      opener = '(';

    /* Missing intermediate closers, or just need something else entirely? */
    {
      Scheme_Object *l;
      Scheme_Indent *indt2;

      missing = "expected";
      for (l = SCHEME_CDR(indentation); SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
	indt2 = (Scheme_Indent *)SCHEME_CAR(l);
	if (indt2->closer == ch) {
	  missing = "missing";
	}
      }
    }

    if (ch == indt->closer) {
      sprintf(found, "unexpected");
    } else if (indt->multiline) {
      sprintf(found,
	      "%s %s to close %s on line %" PRIdPTR ", found instead",
	      missing,
	      closer_name(params, indt->closer),
	      opener_name(params, opener),
	      indt->start_line);
    } else {
      sprintf(found,
	      "%s %s to close preceding %s, found instead",
	      missing,
	      closer_name(params, indt->closer),
	      opener_name(params, opener));
    }

    if (indt->suspicious_line) {
      suggestion = scheme_malloc_atomic(100);
      sprintf(suggestion,
	      "; indentation suggests a missing %s before line %" PRIdPTR,
	      closer_name(params, indt->suspicious_closer),
	      indt->suspicious_line);
    }
  }

  scheme_read_err(port, stxsrc, line, col, pos, 1, 0, indentation, "read: %s `%c'%s",
		  found, ch, suggestion);
}

static void pop_indentation(Scheme_Object *indentation)
{
  /* Pop off indentation stack, and propagate
     suspicions if none found earlier. */
  if (SCHEME_PAIRP(indentation)) {
    Scheme_Indent *indt;
    indt = (Scheme_Indent *)SCHEME_CAR(indentation);
    indentation = SCHEME_CDR(indentation);
    if (SCHEME_PAIRP(indentation)) {
      Scheme_Indent *old_indt;
      old_indt = (Scheme_Indent *)SCHEME_CAR(indentation);

      if (!old_indt->suspicious_line) {
	if (indt->suspicious_line) {
	  old_indt->suspicious_line = indt->suspicious_line;
	  old_indt->suspicious_closer = indt->suspicious_closer;
	}
      }
      if (!old_indt->suspicious_quote) {
	if (indt->suspicious_quote) {
	  old_indt->suspicious_quote = indt->suspicious_quote;
	}
      }
    }
  }
}

/*========================================================================*/
/*                               .zo reader                               */
/*========================================================================*/

typedef struct Scheme_Load_Delay {
  MZTAG_IF_REQUIRED
  Scheme_Object *path;
  intptr_t file_offset, size;
  uintptr_t symtab_size;
  Scheme_Object **symtab;
  intptr_t *shared_offsets;
  Scheme_Hash_Table *symtab_entries; /* `symtab` content to be skipped by resolve_references */
  Scheme_Object *relto;
  Scheme_Unmarshal_Tables *ut;
  struct CPort *current_rp;
  int perma_cache;
  unsigned char *cached;
  Scheme_Object *cached_port;
  struct Scheme_Load_Delay *clear_bytes_prev;
  struct Scheme_Load_Delay *clear_bytes_next;
  int unsafe_ok;
  mzlonglong bytecode_hash;
} Scheme_Load_Delay;

#define ZO_CHECK(x) if (!(x)) scheme_ill_formed_code(port);
#define RANGE_CHECK(x, y) ZO_CHECK (x y)
#define RANGE_POS_CHECK(x, y) ZO_CHECK ((x > 0) && (x y))
#define RANGE_CHECK_GETS(x) RANGE_CHECK(x, <= port->size - port->pos)

typedef struct CPort {
  MZTAG_IF_REQUIRED
  uintptr_t pos, size;
  unsigned char *start;
  uintptr_t symtab_size;
  intptr_t base;
  int unsafe_ok;
  Scheme_Object *orig_port;
  Scheme_Hash_Table **ht;
  Scheme_Object *symtab_refs;
  Scheme_Unmarshal_Tables *ut;
  Scheme_Object **symtab;
  Scheme_Hash_Table *symtab_entries;
  Scheme_Object *magic_sym, *magic_val;
  Scheme_Object *relto;
  intptr_t *shared_offsets;
  Scheme_Load_Delay *delay_info;
  mzlonglong bytecode_hash;
} CPort;
#define CP_GETC(cp) ((int)(cp->start[cp->pos++]))
#define CP_TELL(port) (port->pos + port->base)

static Scheme_Object *read_marshalled(int type, CPort *port);
static Scheme_Object *read_compact_list(int c, int proper, int use_stack, CPort *port);
static Scheme_Object *read_compact_quote(CPort *port, int embedded);

void scheme_ill_formed(struct CPort *port
#if TRACK_ILL_FORMED_CATCH_LINES
		       , const char *file, int line
#endif
		       )
{
  scheme_read_err(port ? port->orig_port : NULL, 
                  NULL, -1, -1, port ? CP_TELL(port) : 0, -1, 0, NULL,
		  "read (compiled): ill-formed code"
#if TRACK_ILL_FORMED_CATCH_LINES
		  " [%s:%d]", file, line
#endif
		  );
}

static void unsafe_disallowed(struct CPort *port)
{
  scheme_read_err(port ? port->orig_port : NULL,
                  NULL, -1, -1, port ? CP_TELL(port) : 0, -1, 0, NULL,
		  "read (compiled): unsafe values disallowed");
}

static void make_ut(CPort *port)
{
  Scheme_Unmarshal_Tables *ut;
  Scheme_Hash_Table *rht;
  char *decoded;

  ut = MALLOC_ONE_RT(Scheme_Unmarshal_Tables);
  SET_REQUIRED_TAG(ut->type = scheme_rt_unmarshal_info);
  port->ut = ut;
  ut->rp = port;
  if (port->delay_info)
    port->delay_info->ut = ut;

  decoded = (char *)scheme_malloc_atomic(port->symtab_size);
  memset(decoded, 0, port->symtab_size);
  ut->decoded = decoded;

  ut->bytecode_hash = port->bytecode_hash;
  
  rht = scheme_make_hash_table(SCHEME_hash_ptr);
  port->ut->rns = rht;

  rht = scheme_make_hash_table(SCHEME_hash_ptr);
  port->ut->multi_scope_pairs = rht;
}

static void prepare_current_unmarshal(Scheme_Unmarshal_Tables *ut)
{
  /* in case a previous unmarshal was interrupted: */
  ut->current_rns = NULL;
  ut->current_multi_scope_pairs = NULL;
}

static void merge_ht(Scheme_Hash_Table *f, Scheme_Hash_Table *t)
{
  int i;
  for (i = f->size; i--; ) {
    if (f->vals[i])
      scheme_hash_set(t, f->keys[i], f->vals[i]);
  }
}

static void complete_current_unmarshal(Scheme_Unmarshal_Tables *ut)
{
  if (ut->current_rns) {
    merge_ht(ut->current_rns, ut->rns);
    ut->current_rns = NULL;
  }
  if (ut->current_multi_scope_pairs) {
    merge_ht(ut->current_multi_scope_pairs, ut->multi_scope_pairs);
    ut->current_multi_scope_pairs = NULL;
  }
}

/* Since read_compact_number is called often, we want it to be
   a cheap call in 3m, so avoid anything that allocated --- even
   error reporting, since we can make up a valid number. */
#define NUM_ZO_CHECK(x) if (!(x)) return 0;

XFORM_NONGCING static intptr_t read_compact_number(CPort *port)
{
  intptr_t flag, v, a, b, c, d;

  NUM_ZO_CHECK(port->pos < port->size);

  flag = CP_GETC(port);

  if (flag < 128)
    return flag;
  else if (!(flag & 0x40)) {
    NUM_ZO_CHECK(port->pos < port->size);

    a = CP_GETC(port);

    v = (flag & 0x3F)
      + (a << 6);
    return v;
  } else if (!(flag & 0x20)) {
    return -(flag & 0x1F);
  }

  NUM_ZO_CHECK(port->pos + 3 < port->size);

  a = CP_GETC(port);
  b = CP_GETC(port);
  c = CP_GETC(port);
  d = CP_GETC(port);

  v = a
    + (b << 8)
    + (c << 16)
    + (d << 24);

  if (flag & 0x10)
    return v;
  else
    return -v;
}

static char *read_compact_chars(CPort *port,
				char *buffer,
				int bsize, int l)
{
  /* Range check is performed before the function is called. */
  char *s;

  if (l < bsize)
    s = buffer;
  else
    s = (char *)scheme_malloc_atomic(l + 1);

  memcpy(s, port->start + port->pos, l);
  port->pos += l;

  s[l] = 0;

  return s;
}

static Scheme_Object *read_compact_svector(CPort *port, int l)
{
  Scheme_Object *o;
  mzshort *v;

  o = scheme_alloc_object();
  o->type = scheme_svector_type;

  SCHEME_SVEC_LEN(o) = l;
  if (l > 0) {
    if (l > 4096) {
      v = (mzshort *)scheme_malloc_fail_ok(scheme_malloc_atomic, 
                                           scheme_check_overflow(l, sizeof(mzshort), 0));
      if (!v)
        scheme_signal_error("out of memory allocating vector");
    } else {
      v = MALLOC_N_ATOMIC(mzshort, l);
    }
  } else {
    v = NULL;
    l = 0; /* in case it was negative */
  }
  SCHEME_SVEC_VEC(o) = v;

  while (l-- > 0) {
    mzshort cn;
    cn = read_compact_number(port);
    v[l] = cn;
  }

  return o;
}

static int valid_utf8(const char *s, int l)
{
  return (scheme_utf8_decode((const unsigned char *)s, 0, l, NULL, 0, -1, NULL, 0, 0) >= 0);
}

  
static Scheme_Object *read_escape_from_string(char *s, intptr_t len,
                                              Scheme_Object *rel_to,
                                              Scheme_Hash_Table **ht)
{
  Scheme_Object *ep;
  ReadParams params;

  ep = scheme_make_sized_byte_string_input_port(s, len);
  
  params.can_read_compiled = 1;
  params.can_read_pipe_quote = 1;
  params.can_read_box = 1;
  params.can_read_graph = 1;
  /* Use startup value of case sensitivity so legacy code will work. */
  params.case_sensitive = scheme_case_sensitive;
  params.square_brackets_are_parens = 1;
  params.curly_braces_are_parens = 1;
  params.square_brackets_are_tagged = 0;
  params.curly_braces_are_tagged = 0;
  params.read_cdot = 0;
  params.read_decimal_inexact = 1;
  params.can_read_dot = 1;
  params.can_read_infix_dot = 1;
  params.can_read_quasi = 1;
  params.skip_zo_vers_check = 0;
  params.table = NULL;

  params.read_relative_path = rel_to;

  return read_inner(ep, NULL, ht, scheme_null, &params, 0);
}

static Scheme_Object *read_compact_escape(CPort *port) 
{
#if defined(MZ_PRECISE_GC)
# define ESC_BLK_BUF_SIZE 32
  char buffer[ESC_BLK_BUF_SIZE];
#endif
  int len;
  char *s;
  
  len = read_compact_number(port);
  
  RANGE_CHECK_GETS((unsigned)len);
  
#if defined(MZ_PRECISE_GC)
  s = read_compact_chars(port, buffer, ESC_BLK_BUF_SIZE, len);
  if (s != buffer)
    len = -len; /* no alloc in sized_byte_string_input_port */
#else
  s = (char *)port->start + port->pos;
  port->pos += len;
  len = -len; /* no alloc in sized_byte_string_input_port */
#endif

  return read_escape_from_string(s, len, port->relto, port->ht);
}

static void record_symtab_self_contained(Scheme_Hash_Table *symtab_entries, Scheme_Object *v)
{
  if (SCHEME_PAIRP(v)
      || SCHEME_BOXP(v)
      || SCHEME_VECTORP(v)
      || SCHEME_HASHTRP(v)
      || SCHEME_STRUCTP(v)) {
    /* Register `v` as a value that is shared through the symbol table,
       so that later calls to resolve_references() can avoid re-traversing
       the value. (Otherwise, bytecode reading can become quadratic-time.) */
    scheme_hash_set(symtab_entries, v, scheme_true);
  }
}

static Scheme_Object *resolve_symtab_refs(Scheme_Object *v, CPort *port)
{
  Scheme_Object *l;

  if (SCHEME_NULLP(port->symtab_refs))
    return v;

  if (v) {
    v = scheme_make_pair(v, port->symtab_refs);
    
    v = resolve_references(v, port->orig_port, NULL,
                           scheme_make_hash_table(SCHEME_hash_ptr), 
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           port->symtab_entries,
                           0, 0);
    
    l = SCHEME_CDR(v);
  } else
    l = port->symtab_refs;

  for (; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
    if (v) {
      port->symtab[SCHEME_INT_VAL(SCHEME_CAR(SCHEME_CAR(l)))] = SCHEME_CDR(SCHEME_CAR(l));
      record_symtab_self_contained(port->symtab_entries, SCHEME_CDR(SCHEME_CAR(l)));
    } else {
      /* interrupted; discard partial constructions */
      port->symtab[SCHEME_INT_VAL(SCHEME_CAR(SCHEME_CAR(l)))] = NULL;
    }
  }
  
  port->symtab_refs = scheme_null;

  if (v)
    return SCHEME_CAR(v);
  else
    return NULL;
}

static Scheme_Object *read_compact(CPort *port, int use_stack);

static Scheme_Object *read_compact_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  CPort *port = (CPort *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return read_compact(port, p->ku.k.i1);
}

/* never a valid symtab value: */
#define SYMTAB_IN_PROGRESS SCHEME_MULTIPLE_VALUES

static Scheme_Object *read_compact(CPort *port, int use_stack)
{
#define BLK_BUF_SIZE 32
  unsigned int l;
  char *s, buffer[BLK_BUF_SIZE];
  int ch;
  Scheme_Object *v;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)port;
      p->ku.k.i1 = use_stack;
      return scheme_handle_stack_overflow(read_compact_k);
    }
  }
#endif

  {
    ZO_CHECK(port->pos < port->size);
    ch = CP_GETC(port);

    switch(cpt_branch[ch]) {
    case CPT_ESCAPE:
      v = read_compact_escape(port);
      break;
    case CPT_SYMBOL:
      l = read_compact_number(port);
      RANGE_CHECK_GETS(l);
      s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
      if (!valid_utf8(s, l))
        scheme_ill_formed_code(port);
      v = scheme_intern_exact_symbol(s, l);

      if (SAME_OBJ(v, port->magic_sym))
	v = port->magic_val;
      break;
    case CPT_SYMREF:
      l = read_compact_number(port);
      RANGE_POS_CHECK(l, < port->symtab_size);
      v = port->symtab[l];
      if (v == SYMTAB_IN_PROGRESS) {
        /* there is a cycle */
        scheme_ill_formed_code(port);
      }
      if (!v) {
        intptr_t save_pos = port->pos;
        port->symtab[l] = SYMTAB_IN_PROGRESS; /* avoid cycles if marshaled form is broken: */
        port->pos = port->shared_offsets[l - 1];
        v = read_compact(port, 0);
        port->pos = save_pos;
        port->symtab[l] = v;
      }
      break;
    case CPT_WEIRD_SYMBOL:
      {
	int uninterned;

	uninterned = read_compact_number(port);

	l = read_compact_number(port);
	RANGE_CHECK_GETS(l);
	s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);

        if (!valid_utf8(s, l))
          scheme_ill_formed_code(port);

	if (uninterned)
	  v = scheme_make_exact_symbol(s, l);
	else
	  v = scheme_intern_exact_parallel_symbol(s, l);
        
	/* The fact that all uses of the symbol go through the table
	   means that uninterned symbols are consistently re-created for
	   a particular compiled expression. */
      }
      break;
    case CPT_KEYWORD:
      l = read_compact_number(port);
      RANGE_CHECK_GETS(l);
      s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
      if (!valid_utf8(s, l))
        scheme_ill_formed_code(port);
      v = scheme_intern_exact_keyword(s, l);
      break;
    case CPT_BYTE_STRING:
      l = read_compact_number(port);
      RANGE_CHECK_GETS(l);
      s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
      v = scheme_make_immutable_sized_byte_string(s, l, l < BLK_BUF_SIZE);
      v = scheme_intern_literal_string(v);
      break;
    case CPT_CHAR_STRING:
      {
	unsigned int el;
	mzchar *us;
	el = read_compact_number(port);
	l = read_compact_number(port);
	RANGE_CHECK_GETS(el);
	s = read_compact_chars(port, buffer, BLK_BUF_SIZE, el);
        if (l < 4096)
          us = (mzchar *)scheme_malloc_atomic((l + 1) * sizeof(mzchar));
        else
          us = (mzchar *)scheme_malloc_fail_ok(scheme_malloc_atomic, (l + 1) * sizeof(mzchar));
        if (scheme_utf8_decode((const unsigned char *)s, 0, el, us, 0, l, NULL, 0, 0) != l)
          scheme_ill_formed_code(port);
	us[l] = 0;
	v = scheme_make_immutable_sized_char_string(us, l, 0);
        v = scheme_intern_literal_string(v);
      }
      break;
    case CPT_CHAR:
      l = read_compact_number(port);
      return make_interned_char(l, scheme_true);
      break;
    case CPT_INT:
      return scheme_make_integer(read_compact_number(port));
      break;
    case CPT_NULL:
      return scheme_null;
      break;
    case CPT_TRUE:
      return scheme_true;
      break;
    case CPT_FALSE:
      return scheme_false;
      break;
    case CPT_VOID:
      return scheme_void;
      break;
    case CPT_BOX:
      v = scheme_box(read_compact(port, 0));
      SCHEME_SET_IMMUTABLE(v);
      break;
    case CPT_PAIR:
      {
	v = read_compact(port, 0);
	return scheme_make_pair(v, read_compact(port, 0));
      }
      break;
    case CPT_LIST:
      l = read_compact_number(port);
      if (l == 1) {
        v = read_compact(port, 0);
        return scheme_make_pair(v, read_compact(port, 0));
      } else
        return read_compact_list(l, 0, 0, port);
      break;
    case CPT_VECTOR:
      {
	Scheme_Object *vec;
	unsigned int i;

	l = read_compact_number(port);
	vec = scheme_make_vector(l, NULL);

	for (i = 0; i < l; i++) {
	  v = read_compact(port, 0);
	  SCHEME_VEC_ELS(vec)[i] = v;
	}

        SCHEME_SET_IMMUTABLE(vec);

	return vec;
      }
      break;
    case CPT_HASH_TABLE:
      {
	Scheme_Object *l;
	int kind, len;
        Scheme_Object *k;

	kind = read_compact_number(port);
	len = read_compact_number(port);
	
	l = scheme_null;
	while (len--) {
	  k = read_compact(port, 0);
	  v = read_compact(port, 0);
	  /* We can't always hash directly, because a key or value
	     might have a graph reference inside it. */
	  l = scheme_make_pair(scheme_make_pair(k, v), l);
	}

        if (!(*port->ht)) {
          /* So that resolve_references is called to build the table: */
          Scheme_Hash_Table *tht;
          tht = scheme_make_hash_table(SCHEME_hash_ptr);
          *(port->ht) = tht;
        }

	/* Let resolve_references complete the table construction: */
        v = scheme_alloc_object();
        v->type = scheme_table_placeholder_type;
        SCHEME_PINT_VAL(v) = kind;
        SCHEME_IPTR_VAL(v) = l;
      }
      break;
    case CPT_STX:
      {
        Scheme_Hash_Table *save_ht;

	if (!port->ut)
          make_ut(port);

        save_ht = *port->ht;
        *port->ht = NULL;

        prepare_current_unmarshal(port->ut);
	v = read_compact(port, 1);

        if (!SCHEME_NULLP(port->symtab_refs))
          v = resolve_symtab_refs(v, port);
        else if (*port->ht) {
          *port->ht = NULL;
          v = resolve_references(v, port->orig_port, NULL,
                                 scheme_make_hash_table(SCHEME_hash_ptr), 
                                 scheme_make_hash_table(SCHEME_hash_ptr),
                                 port->symtab_entries,
                                 0, 0);
        }

        *port->ht = save_ht;

	v = scheme_unmarshal_datum_to_syntax(v, port->ut, 0);
	scheme_num_read_syntax_objects++;
	if (!v)
	  scheme_ill_formed_code(port);
        complete_current_unmarshal(port->ut);
      }
      break;
    case CPT_MARSHALLED:
      v = read_marshalled(read_compact_number(port), port);
      break;
    case CPT_QUOTE:
      v = read_compact_quote(port, 1);
      break;
    case CPT_REFERENCE:
      l = read_compact_number(port);
      RANGE_CHECK(l, < (EXPECTED_PRIM_COUNT 
                        + EXPECTED_UNSAFE_COUNT 
                        + EXPECTED_FLFXNUM_COUNT 
                        + EXPECTED_EXTFL_COUNT
                        + EXPECTED_FUTURES_COUNT
                        + EXPECTED_FOREIGN_COUNT));
      if ((l >= unsafe_variable_references_start)
          && !port->unsafe_ok)
        unsafe_disallowed(port);
      return variable_references[l];
      break;
    case CPT_LOCAL:
      {
	int p, flags;
	p = read_compact_number(port);
        if (p < 0) {
          p = -(p + 1);
          flags = read_compact_number(port);
        } else
          flags = 0;
	return scheme_make_local(scheme_local_type, p, flags);
      }
      break;
    case CPT_LOCAL_UNBOX:
      {
	int p, flags;
	p = read_compact_number(port);
        if (p < 0) {
          p = -(p + 1);
          flags = read_compact_number(port);
        } else
          flags = 0;
	return scheme_make_local(scheme_local_unbox_type, p, flags);
      }
      break;
    case CPT_SVECTOR:
      {
	int l;
	l = read_compact_number(port);
	v = read_compact_svector(port, l);
      }
      break;
    case CPT_APPLICATION:
      {
	int c, i;
	Scheme_App_Rec *a;

	c = read_compact_number(port) + 1;

	a = scheme_malloc_application(c);
	for (i = 0; i < c; i++) {
	  v = read_compact(port, 1);
	  a->args[i] = v;
	}

	scheme_finish_application(a);
	return (Scheme_Object *)a;
      }
      break;
    case CPT_LET_ONE:
    case CPT_LET_ONE_TYPED:
    case CPT_LET_ONE_UNUSED:
      {
	Scheme_Let_One *lo;
	int et;

	lo = (Scheme_Let_One *)scheme_malloc_tagged(sizeof(Scheme_Let_One));
	lo->iso.so.type = scheme_let_one_type;

	v = read_compact(port, 1);
        lo->value = v;
	v = read_compact(port, 1);
	lo->body = v;
	et = scheme_get_eval_type(lo->value);
        if (ch == CPT_LET_ONE_TYPED) {
          int ty;
          ty = read_compact_number(port);
          et |= (ty << LET_ONE_TYPE_SHIFT);
        } else if (ch == CPT_LET_ONE_UNUSED)
          et |= LET_ONE_UNUSED;
        SCHEME_LET_EVAL_TYPE(lo) = et;
	
	return (Scheme_Object *)lo;
      }
      break;
    case CPT_BRANCH:
      {
	Scheme_Object *test, *tbranch, *fbranch;
	test = read_compact(port, 1);
	tbranch = read_compact(port, 1);
	fbranch = read_compact(port, 1);
	return scheme_make_branch(test, tbranch, fbranch);
      }
      break;
    case CPT_MODULE_INDEX:
	{
	  Scheme_Object *path, *base;

	  path = read_compact(port, 0);
	  base = read_compact(port, 0);
          if (SCHEME_FALSEP(path)
              && SCHEME_FALSEP(base)) {
            path = read_compact(port, 0);
            if (SCHEME_FALSEP(path))
              return scheme_make_modidx(scheme_false, scheme_false, scheme_false);
            else
              return scheme_get_submodule_empty_self_modidx(path, 0);
          } else
            return scheme_make_modidx(path, base, scheme_false);
	}
	break;
    case CPT_MODULE_VAR:
      {
	Module_Variable *mv;
	Scheme_Object *mod, *var, *shape;
	int pos;

	mod = read_compact(port, 0);
	var = read_compact(port, 0);
	shape = read_compact(port, 0);
	pos = read_compact_number(port);

	mv = MALLOC_ONE_TAGGED(Module_Variable);
	mv->iso.so.type = scheme_module_variable_type;
        if (SCHEME_SYMBOLP(mod))
          mod = scheme_intern_resolved_module_path(mod);
	mv->modidx = mod;
	mv->sym = var;
	mv->shape = shape;
        if (pos < -3) {
          pos = -(pos + 3);
          SCHEME_MODVAR_FLAGS(mv) = pos;
          pos = read_compact_number(port);
        }
        if (pos == -2) {
          pos = read_compact_number(port);
          mv->mod_phase = pos;
          pos = read_compact_number(port);
          mv->pos = pos;
        } else
          mv->pos = pos;

	return (Scheme_Object *)mv;
      }
      break;
    case CPT_PATH:
      {
	l = read_compact_number(port);
	RANGE_CHECK_GETS(l);
        if (l) {
          s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
          v = scheme_make_sized_path(s, l, l < BLK_BUF_SIZE);
        } else {
          Scheme_Object *elems;
          elems = read_compact(port, 0);
          if (SCHEME_PATHP(port->relto)) {
            /* Resolve relative path using the current load-relative directory: */
            v = port->relto;
          } else
            v = scheme_maybe_build_path(NULL, scheme_false);
          while (SCHEME_PAIRP(elems)) {
            v = scheme_maybe_build_path(v, SCHEME_CAR(elems));
            elems = SCHEME_CDR(elems);
          }
        }
      }
      break;
    case CPT_CLOSURE:
      {
        Scheme_Closure *cl;
        l = read_compact_number(port);
        RANGE_CHECK(l, < port->symtab_size);
        cl = scheme_malloc_empty_closure();
        port->symtab[l] = (Scheme_Object *)cl;
        v = read_compact(port, 0);
        if (!SAME_TYPE(SCHEME_TYPE(v), scheme_closure_type)
            || !((Scheme_Closure *)v)->code
            || ((Scheme_Closure *)v)->code->closure_size) {
          scheme_ill_formed_code(port);
          return NULL;
        }
        cl->code = ((Scheme_Closure *)v)->code;
        return (Scheme_Object *)cl;
        break;
      }
    case CPT_DELAY_REF:
      {
        l = read_compact_number(port);
        RANGE_POS_CHECK(l, < port->symtab_size);
        v = port->symtab[l];
        if (!v) {
          if (port->delay_info) {
            /* This is where we construct information for
               loading the syntax object on demand. */
            v = scheme_make_raw_pair(scheme_make_integer(l),
                                     (Scheme_Object *)port->delay_info);
          } else {
            intptr_t save_pos = port->pos;
            port->symtab[l] = SYMTAB_IN_PROGRESS; /* avoid cycles if marshaled form is broken: */
            port->pos = port->shared_offsets[l - 1];
            v = read_compact(port, 0);
            port->pos = save_pos;
            port->symtab[l] = v;
          }
        } else if (v == SYMTAB_IN_PROGRESS) {
          /* there is a cycle */
          scheme_ill_formed_code(port);
        }
        return v;
        break;
      }
    case CPT_PREFAB:
      {
        Scheme_Struct_Type *st;
        v = read_compact(port, 0);
        if (!SCHEME_VECTORP(v) || !SCHEME_VEC_SIZE(v))
          v = NULL;
        else {
          st = scheme_lookup_prefab_type(SCHEME_VEC_ELS(v)[0], SCHEME_VEC_SIZE(v) - 1);
          if (!st || (st->num_slots != (SCHEME_VEC_SIZE(v) - 1)))
            v = NULL;
          else {
            v = scheme_make_prefab_struct_instance(st, v);
          }
        }
        break;
      }
    case CPT_SMALL_LOCAL_START:
    case CPT_SMALL_LOCAL_UNBOX_START:
      {
	Scheme_Type type;

	if (CPT_BETWEEN(ch, SMALL_LOCAL_UNBOX)) {
	  type = scheme_local_unbox_type;
	  ch -= CPT_SMALL_LOCAL_UNBOX_START;
	} else {
	  type = scheme_local_type;
	  ch -= CPT_SMALL_LOCAL_START;
	}
	return scheme_make_local(type, ch, 0);
      }
      break;
    case CPT_SMALL_MARSHALLED_START:
      {
	l = ch - CPT_SMALL_MARSHALLED_START;
	v = read_marshalled(l, port);
      }
      break;
    case CPT_SMALL_SYMBOL_START:
      {
	l = ch - CPT_SMALL_SYMBOL_START;
	RANGE_CHECK_GETS(l);
	s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
        if (!valid_utf8(s, l))
          scheme_ill_formed_code(port);
	v = scheme_intern_exact_symbol(s, l);

	if (SAME_OBJ(v, port->magic_sym))
	  v = port->magic_val;
      }
      break;
    case CPT_SMALL_NUMBER_START:
      {
	l = ch - CPT_SMALL_NUMBER_START;
	return scheme_make_integer(l);
      }
      break;
    case CPT_SMALL_SVECTOR_START:
      {
	l = ch - CPT_SMALL_SVECTOR_START;
	v = read_compact_svector(port, l);
      }
      break;
    case CPT_SMALL_PROPER_LIST_START:
    case CPT_SMALL_LIST_START:
      {
	int ppr = CPT_BETWEEN(ch, SMALL_PROPER_LIST);
	l = ch - (ppr ? CPT_SMALL_PROPER_LIST_START : CPT_SMALL_LIST_START);
      	if (l == 1) {
          Scheme_Object *cdr;
          v = read_compact(port, 0);
          cdr = (ppr
                 ? scheme_null
                 : read_compact(port, 0));
          return scheme_make_pair(v, cdr);
        } else
          return read_compact_list(l, ppr, /* use_stack */ 0, port);
      }
      break;
    case CPT_SMALL_APPLICATION_START:
      {
	int c, i;
	Scheme_App_Rec *a;

	c = (ch - CPT_SMALL_APPLICATION_START) + 1;

	a = scheme_malloc_application(c);
	for (i = 0; i < c; i++) {
	  v = read_compact(port, 1);
	  a->args[i] = v;
	}

	scheme_finish_application(a);

	return (Scheme_Object *)a;
      }
      break;
    case CPT_SMALL_APPLICATION2:
      {
	short et;
	Scheme_App2_Rec *app;

	app = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
	app->iso.so.type = scheme_application2_type;

	v = read_compact(port, 1);
	app->rator = v;
	v = read_compact(port, 1);
	app->rand = v;

	et = scheme_get_eval_type(app->rand);
	et = et << 3;
	et += scheme_get_eval_type(app->rator);
	SCHEME_APPN_FLAGS(app) = et;

	return (Scheme_Object *)app;
      }
      break;
    case CPT_SMALL_APPLICATION3:
      {
	short et;
	Scheme_App3_Rec *app;

	app = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
	app->iso.so.type = scheme_application3_type;

	v = read_compact(port, 1);
	app->rator = v;
	v = read_compact(port, 1);
	app->rand1 = v;
	v = read_compact(port, 1);
	app->rand2 = v;

	et = scheme_get_eval_type(app->rand2);
	et = et << 3;
	et += scheme_get_eval_type(app->rand1);
	et = et << 3;
	et += scheme_get_eval_type(app->rator);
	SCHEME_APPN_FLAGS(app) = et;

	return (Scheme_Object *)app;
      }
      break;
    case CPT_SCOPE:
      {
        Scheme_Object *v2;

        if (!port->ut)
          make_ut(port);

        v = scheme_box(scheme_false);
        l = read_compact_number(port);

        if (l) {
          RANGE_POS_CHECK(l, < port->symtab_size);
          port->symtab[l] = v;
        }

        l = read_compact_number(port);
        
        v2 = read_compact(port, 0);
        v2 = scheme_make_pair(scheme_make_integer(l), v2);
        SCHEME_BOX_VAL(v) = v2;

        return v;
      }
      break;
    case CPT_ROOT_SCOPE:
      return scheme_stx_root_scope();
    case CPT_SHARED:
      {
        Scheme_Object *ph;
        
        if (!port->ut)
          make_ut(port);

        ph = scheme_alloc_small_object();
        ph->type = scheme_placeholder_type;
        SCHEME_PTR_VAL(ph) = scheme_false;
        
        l = read_compact_number(port);
        RANGE_POS_CHECK(l, < port->symtab_size);
        
        port->symtab[l] = ph;

        v = scheme_make_pair(scheme_make_pair(scheme_make_integer(l),
                                              ph),
                             port->symtab_refs);
        port->symtab_refs = v;

        v = read_compact(port, 0);
        SCHEME_PTR_VAL(ph) = v;

        return ph;
      }
      break;
    default:
      v = NULL;
      break;
    }

    /* Some cases where v != NULL return directly */

    if (!v)
      scheme_ill_formed_code(port);
  }

  return v;
}

static Scheme_Object *read_compact_list(int c, int proper, int use_stack, CPort *port)
{
  Scheme_Object *v, *first, *last, *pair;

  v = read_compact(port, 0);
  last = scheme_make_pair(v, scheme_null);

  first = last;

  while (--c) {
    v = read_compact(port, 0);

    pair = scheme_make_pair(v, scheme_null);

    SCHEME_CDR(last) = pair;
    last = pair;
  }

  if (!proper) {
    v = read_compact(port, 0);
    SCHEME_CDR(last) = v;
  }

  return first;
}

static Scheme_Object *read_compact_quote(CPort *port, int embedded)
{
  Scheme_Hash_Table **q_ht, **old_ht;
  Scheme_Object *v;

  /* Use a new hash table. A compiled quoted form may have graph
     structure, but only local graph structure is allowed. */
  q_ht = MALLOC_N(Scheme_Hash_Table *, 1);
  *q_ht = NULL;

  old_ht = port->ht;
  port->ht = q_ht;

  v = read_compact(port, 0);

  port->ht = old_ht;

  if (*q_ht)
    v = resolve_references(v, port->orig_port, NULL,
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           port->symtab_entries,
                           0, 0);

  return v;
}

static Scheme_Object *read_marshalled(int type, CPort *port)
{
  Scheme_Object *l;
  Scheme_Type_Reader reader;

  l = read_compact(port, 1);

  if ((type < 0) || (type >= _scheme_last_type_)) {
    scheme_ill_formed_code(port);
  }

  reader = scheme_type_readers[type];

  if (!reader) {
    scheme_ill_formed_code(port);
  }

  l = reader(l);

  if (!l)
    scheme_ill_formed_code(port);

  return l;
}

static intptr_t read_simple_number_from_port(Scheme_Object *port)
{
  intptr_t a, b, c, d;

  a = (unsigned char)scheme_get_byte(port);
  b = (unsigned char)scheme_get_byte(port);
  c = (unsigned char)scheme_get_byte(port);
  d = (unsigned char)scheme_get_byte(port);

  return (a
          + (b << 8)
          + (c << 16)
          + (d << 24));
}

static void install_byecode_hash_code(CPort *rp, char *hash_code)
{
  mzlonglong l = 0;
  int i;

  for (i = 0; i < 20; i++) {
    l ^= ((umzlonglong)(hash_code[i]) << ((i % 8) * 8));
  }

  /* Make sure the hash code leaves lots of room for
     run-time generated indices: */
# define LARGE_SPAN ((mzlonglong)1 << 40)

  if (!l) l = LARGE_SPAN;
  if (l > 0) l = -l;
  if (l > (-LARGE_SPAN)) l -= LARGE_SPAN;
  rp->bytecode_hash = l;
}

char *scheme_submodule_path_to_string(Scheme_Object *p, intptr_t *_len)
{
  Scheme_Object *pr;
  intptr_t len = 0, l;
  unsigned char *s;

  for (pr = p; !SCHEME_NULLP(pr); pr = SCHEME_CDR(pr)) {
    l = SCHEME_SYM_LEN(SCHEME_CAR(pr));
    if (l < 255)
      len += l + 1;
    else
      len += l + 1 + 4;
  }
  *_len = len;

  s = scheme_malloc_atomic(len + 1);
  s[len] = 0;
  
  len = 0;
  for (pr = p; !SCHEME_NULLP(pr); pr = SCHEME_CDR(pr)) {
    l = SCHEME_SYM_LEN(SCHEME_CAR(pr));
    if (l < 255) {
      s[len++] = l;
    } else {
      s[len++] = 255;
      s[len++] = (l & 0xFF);
      s[len++] = ((l >> 8) & 0xFF);
      s[len++] = ((l >> 16) & 0xFF);
      s[len++] = ((l >> 24) & 0xFF);
    }
    memcpy(s + len, SCHEME_SYM_VAL(SCHEME_CAR(pr)), l);
    len += l;
  }

  return (char *)s;
}

Scheme_Object *scheme_string_to_submodule_path(char *_s, intptr_t len)
{
  unsigned char *s = (unsigned char *)_s;
  char *e, buffer[32];
  uintptr_t pos = 0, l;
  Scheme_Object *first = NULL, *last = NULL, *pr;

  while (pos < len) {
    l = s[pos++];
    if ((l == 255) && ((len - pos) > 4)) {
      l = (s[pos] | (s[pos+1] << 8) | (s[pos+2] << 16) | (s[pos+3] << 24));
      pos += 4;
    }
    if (l > len - pos)
      l = len - pos;
    if (l < 32)
      e = buffer;
    else
      e = scheme_malloc_atomic(l + 1);
    memcpy(e, s + pos, l);
    e[l] = 0;
    pos += l;

    if (!valid_utf8(e, l))
      return scheme_null;

    pr = scheme_make_pair(scheme_intern_exact_symbol(e, l), scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;
  }

  return first ? first : scheme_null;
}

static void read_module_directory(Scheme_Object *port, Scheme_Hash_Table *ht, int depth)
{
  char *s;
  Scheme_Object *v, *p;
  int len, left, right;
  intptr_t got;

  if (depth > 32)
    scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                    "read (compiled): multi-module directory tree is imbalanced");
  
  len = read_simple_number_from_port(port);
  if (len < 0) 
    scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                    "read (compiled): directory module name read failed");

  s = scheme_malloc_atomic(len + 1);
  got = scheme_get_bytes(port, len, s, 0);

  if (got != len)
    v = NULL;
  else {
    s[len] = 0;
    v = scheme_string_to_submodule_path(s, len);
    for (p = v; !SCHEME_NULLP(p); p = SCHEME_CDR(p)) {
      if (!SCHEME_SYMBOLP(SCHEME_CAR(p))) {
        v = NULL;
        break;
      }
    }
    if (v && scheme_hash_get(ht, v))
      v = NULL;
  }

  if (!v)
    scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                    "read (compiled): directory module name read failed");

  scheme_hash_set(ht, v, scheme_null);

  (void)read_simple_number_from_port(port); /* offset */
  (void)read_simple_number_from_port(port); /* length */

  left = read_simple_number_from_port(port);
  right = read_simple_number_from_port(port);

  if (left)
    read_module_directory(port, ht, depth+1);
  if (right)
    read_module_directory(port, ht, depth+1);
}

/* "#~" has been read */
static Scheme_Object *read_compiled(Scheme_Object *port,
				    Scheme_Object *stxsrc,
				    intptr_t line, intptr_t col, intptr_t pos,
				    Scheme_Hash_Table **ht,
				    ReadParams *params)
{
  Scheme_Hash_Table *directory = NULL;
  Scheme_Object *result;
  intptr_t size, shared_size, got, offset, directory_count = 0;
  CPort *rp;
  intptr_t symtabsize;
  Scheme_Object **symtab;
  intptr_t *so;
  Scheme_Load_Delay *delay_info;
  Scheme_Hash_Table **local_ht;
  int all_short, mode;
  int perma_cache = use_perma_cache;
  Scheme_Object *dir;
  Scheme_Config *config;
  char hash_code[20];
	  
  while (1) {
    /* Check version: */
    size = scheme_get_byte(port);
    {
      char buf[64];
      
      if (size < 0) size = 0;
      if (size > 63) size = 63;
      
      got = scheme_get_bytes(port, size, buf, 0);
      buf[got] = 0;
      
      if (!params->skip_zo_vers_check)
        if (strcmp(buf, MZSCHEME_VERSION))
          scheme_read_err(port, stxsrc, line, col, pos, got, 0, NULL,
                          "read (compiled): wrong version for compiled code\n"
                          "  compiled version: %s\n"
                          "  expected version: %s",
                          (buf[0] ? buf : "???"), MZSCHEME_VERSION);
    }
    
    mode = scheme_get_byte(port);
    if (mode == 'D') {
      /* a module with submodules, starting with a directory */
      if (directory)
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): found multi-module directory after directory");
      (void)read_simple_number_from_port(port); /* count */
      directory = scheme_make_hash_table_equal();
      read_module_directory(port, directory, 0);
    } else if (mode == 'T') {
      /* single module or other top-level form */
      
      /* Allow delays? */
      if (params->delay_load_info) {
        delay_info = MALLOC_ONE_RT(Scheme_Load_Delay);
        SET_REQUIRED_TAG(delay_info->type = scheme_rt_delay_load_info);
        delay_info->path = params->delay_load_info;
      } else
        delay_info = NULL;

      /* Module hash code */
      got = scheme_get_bytes(port, 20, hash_code, 0);

      symtabsize = read_simple_number_from_port(port);
  
      /* Load table mapping symtab indices to stream positions: */

      all_short = scheme_get_byte(port);
      if (symtabsize < 0)
        so = NULL;
      else
        so = (intptr_t *)scheme_malloc_fail_ok(scheme_malloc_atomic, 
                                               scheme_check_overflow(symtabsize, sizeof(intptr_t), 0));
      if (!so)
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): could not allocate symbol table of size %" PRIdPTR,
                        symtabsize);
      if ((got = scheme_get_bytes(port, (all_short ? 2 : 4) * (symtabsize - 1), (char *)so, 0)) 
          != ((all_short ? 2 : 4) * (symtabsize - 1)))
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): ill-formed code (bad table count: %" PRIdPTR " != %" PRIdPTR ")",
                        got, (all_short ? 2 : 4) * (symtabsize - 1));
      {
        /* This loop runs top to bottom, since sizeof(long) may be larger
           than the decoded integers (but it's never shorter) */
        intptr_t j, v;
        unsigned char *so_c = (unsigned char *)so;
        for (j = symtabsize - 1; j--; ) {
          if (all_short) {
            v = so_c[j * 2]
              + (so_c[j * 2 + 1] << 8);
          } else {
            v = so_c[j * 4]
              + (so_c[j * 4 + 1] << 8)
              + (so_c[j * 4 + 2] << 16)
              + (so_c[j * 4 + 3] << 24);
          }
          so[j] = v;
        }
      }

      /* Continue reading content */

      shared_size = read_simple_number_from_port(port);
      size = read_simple_number_from_port(port);

      if (shared_size >= size) {
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): ill-formed code (shared size %ld >= total size %ld)",
                        shared_size, size);
      }

      rp = MALLOC_ONE_RT(CPort);
      SET_REQUIRED_TAG(rp->type = scheme_rt_compact_port);
      {
        unsigned char *st;
        st = (unsigned char *)scheme_malloc_fail_ok(scheme_malloc_atomic, size + 1);
        rp->start = st;
      }
      rp->pos = 0;
      {
        intptr_t base;
        scheme_tell_all(port, NULL, NULL, &base);
        rp->base = base;
      }
      offset = SCHEME_INT_VAL(scheme_file_position(1, &port));
      rp->orig_port = port;
      rp->size = size;
      if ((got = scheme_get_bytes(port, size, (char *)rp->start, 0)) != size)
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): ill-formed code (bad count: %ld != %ld"
                        ", started at %ld)",
                        got, size, rp->base);

      local_ht = MALLOC_N(Scheme_Hash_Table *, 1);

      symtab = MALLOC_N(Scheme_Object *, symtabsize);
      rp->symtab_size = symtabsize;
      rp->ht = local_ht;
      rp->symtab = symtab;
      rp->unsafe_ok = params->can_read_unsafe;

      {
        Scheme_Hash_Table *se_ht;
        se_ht = scheme_make_hash_table(SCHEME_hash_ptr);
        rp->symtab_entries = se_ht;
        if (delay_info)
          delay_info->symtab_entries = se_ht;
      }

      config = scheme_current_config();

      dir = scheme_get_param(config, MZCONFIG_LOAD_DIRECTORY);
      if (SCHEME_TRUEP(dir))
        dir = scheme_path_to_directory_path(dir);
      rp->relto = dir;

      rp->magic_sym = params->magic_sym;
      rp->magic_val = params->magic_val;

      install_byecode_hash_code(rp, hash_code);

      rp->shared_offsets = so;
      rp->delay_info = delay_info;

      rp->symtab_refs = scheme_null;

      if (!delay_info) {
        /* Read shared parts: */
        intptr_t j, len;
        Scheme_Object *v;
        len = symtabsize;
        for (j = 1; j < len; j++) {
          if (!symtab[j]) {
            v = read_compact(rp, 0);
            v = resolve_symtab_refs(v, rp);
            symtab[j] = v;
          } else {
            if (j+1 < len)
              rp->pos = so[j];
            else
              rp->pos = shared_size;
          }
        }
      } else {
        scheme_reserve_file_descriptor();
        rp->pos = shared_size; /* skip shared part */
        delay_info->file_offset = offset;
        delay_info->size = shared_size;
        delay_info->symtab_size = rp->symtab_size;
        delay_info->symtab = rp->symtab;
        delay_info->shared_offsets = rp->shared_offsets;
        delay_info->relto = rp->relto;
        delay_info->unsafe_ok = rp->unsafe_ok;
        delay_info->bytecode_hash = rp->bytecode_hash;

        if (SAME_OBJ(delay_info->path, scheme_true))
          perma_cache = 1;

        if (perma_cache) {
          unsigned char *cache;
          cache = (unsigned char *)scheme_malloc_atomic(shared_size);
          memcpy(cache, rp->start, shared_size);
          delay_info->cached = cache;
          delay_info->cached_port = port;
          delay_info->perma_cache = 1;
        }
      }

      /* Read main body: */
      result = read_marshalled(scheme_compilation_top_type, rp);

      if (delay_info)
        if (delay_info->ut)
          delay_info->ut->rp = NULL; /* clean up */

      if (*local_ht) {
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "read (compiled): ill-formed code (unexpected graph structure)");
        return NULL;
      }

      if (SAME_TYPE(SCHEME_TYPE(result), scheme_compilation_top_type)) {
        Scheme_Compilation_Top *top = (Scheme_Compilation_Top *)result;

        scheme_validate_code(rp, top->code,
                             top->max_let_depth,
                             top->prefix->num_toplevels,
                             top->prefix->num_stxes,
                             top->prefix->num_lifts,
                             NULL,
                             NULL,
                             0);
        /* If no exception, the resulting code is ok. */

        /* Install module hash code, if any. This code is used to register
           the module in scheme_module_execute(), and it's used to
           find a registered module in the default load handler. */
        {
          int i;
          for (i = 0; i < 20; i++) {
            if (hash_code[i]) break;
          }

          if (i < 20) {
            Scheme_Module *m;
            m = scheme_extract_compiled_module(result);
            if (m) {
              Scheme_Object *hc;
              hc = scheme_make_sized_byte_string(hash_code, 20, 1);
              hc = scheme_make_pair(hc, dir);

              m->code_key = hc;
            }
          }
        }
      } else
        scheme_ill_formed_code(rp);
    
      if (directory) {
        Scheme_Module *m, *m2;
        Scheme_Object *v;
        m = scheme_extract_compiled_module(result);
        if (m) {
          v = scheme_hash_get(directory, m->submodule_path);
          if (v && (SCHEME_NULLP(v) || SCHEME_PAIRP(v))) {
            directory_count++;
            v = scheme_reverse(v);
            m->pre_submodules = v;
            scheme_hash_set(directory, m->submodule_path, result);
            if (!SCHEME_NULLP(m->submodule_path)) {
              /* find parent: */
              v = scheme_reverse(m->submodule_path);
              v = scheme_reverse(SCHEME_CDR(v));
              result = scheme_hash_get(directory, v);
              if (!result)
                scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                                "read (compiled): no parent module found in multi-module stream");
              if (SCHEME_NULLP(result) || SCHEME_PAIRP(result)) {
                /* this is a pre-submodule */
                result = scheme_make_pair((Scheme_Object *)m, result);
                scheme_hash_set(directory, v, result);
              } else {
                /* this is a post-submodule */
                m2 = scheme_extract_compiled_module(result);
                v = m2->post_submodules ? m2->post_submodules : scheme_null;
                v = scheme_make_pair((Scheme_Object *)m, v);
                m2->post_submodules = v;
              }
            }
            if (directory->count == directory_count) {
              /* need to reverse post-submodule lists in all modules: */
              int i;
              for (i = 0; i < directory->size; i++) {
                if (directory->vals[i]) {
                  m = scheme_extract_compiled_module(directory->vals[i]);
                  if (m->post_submodules) {
                    v = scheme_reverse(m->post_submodules);
                    m->post_submodules = v;
                  }
                }
              }

              /* return the root module: */
              return scheme_hash_get(directory, scheme_null);
            }
            /* otherwise, keep reading modules */
          } else
            scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                            "read (compiled): found unrecognized or duplicate module after multi-module directory: %V",
                            m->submodule_path);
        } else
          scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                          "read (compiled): found non-module code after multi-module directory");
      } else
        return result;
    } else {
      scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                      "read (compiled): found bad mode");
    }
    
    
    if ((scheme_get_byte(port) != '#')
        || (scheme_get_byte(port) != '~'))
      scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                      "read (compiled): no `#~' for next module in multi-module stream");
  }
}


THREAD_LOCAL_DECL(static Scheme_Load_Delay *clear_bytes_chain);

void scheme_clear_delayed_load_cache()
{
  Scheme_Load_Delay *next;

  while (clear_bytes_chain) {
    next = clear_bytes_chain->clear_bytes_next;
    clear_bytes_chain->cached = NULL;
    clear_bytes_chain->cached_port = NULL;
    clear_bytes_chain->clear_bytes_next = NULL;
    clear_bytes_chain->clear_bytes_prev = NULL;
    clear_bytes_chain = next;
  }
}

Scheme_Object *scheme_load_delayed_code(int _which, Scheme_Load_Delay *_delay_info)
{
  Scheme_Load_Delay * volatile delay_info = _delay_info;
  CPort *rp;
  CPort * volatile old_rp;
  volatile int which = _which;
  intptr_t size, got;
  unsigned char *st;
  Scheme_Object * volatile port;
  Scheme_Object * volatile v;
  Scheme_Object * volatile v_exn;
  Scheme_Hash_Table ** volatile ht;
  mz_jmp_buf newbuf, * volatile savebuf;

  /* Remove from cache-clearing chain: */
  if (!delay_info->perma_cache) {
    if (delay_info->clear_bytes_prev)
      delay_info->clear_bytes_prev->clear_bytes_next = delay_info->clear_bytes_next;
    else if (clear_bytes_chain == delay_info)
      clear_bytes_chain = delay_info->clear_bytes_next;
    if (delay_info->clear_bytes_next)
      delay_info->clear_bytes_next->clear_bytes_prev = delay_info->clear_bytes_prev;
    
    delay_info->clear_bytes_prev = NULL;
    delay_info->clear_bytes_next = NULL;
  }

  size = delay_info->size;

  /* Load in file bytes: */
  if (!delay_info->cached) {
    Scheme_Object *a[1];

    scheme_start_atomic();
    scheme_release_file_descriptor();

    a[0] = delay_info->path;
    port = scheme_do_open_input_file("on-demand-loader", 0, 1, a, 0, NULL, NULL, 0);

    savebuf = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &newbuf;
    if (scheme_setjmp(newbuf)) {
      scheme_end_atomic_no_swap();
      scheme_close_input_port(port);
      scheme_current_thread->error_buf = savebuf;
      scheme_longjmp(*savebuf, 1);
      return NULL;
    } else {
      st = (unsigned char *)scheme_malloc_atomic(size + 1);

      scheme_set_file_position(port, delay_info->file_offset);
      
      if ((got = scheme_get_bytes(port, size, (char *)st, 0)) != size)
        scheme_read_err(port, NULL, -1, -1, -1, -1, 0, NULL,
                        "on-demand load: ill-formed code (bad count: %ld != %ld"
                        ", started at %ld)",
                        got, size, 0);
    }
    scheme_current_thread->error_buf = savebuf;

    scheme_close_input_port(port);
    scheme_reserve_file_descriptor();

    scheme_end_atomic_no_swap();

    delay_info->cached = st;
    delay_info->cached_port = port;
  } else {
    port = delay_info->cached_port;
  }

  /* Allow only one thread at a time. This is a little questionable,
     because unmarshalling could take arbitrarily long, and an
     untrusted program might construct an adversarial bytecode. That
     would be relatively difficult, though. In practice, unmarshalling
     will be fast. */
  scheme_start_atomic();

  old_rp = delay_info->current_rp;

  /* Create a port for reading: */
  rp = MALLOC_ONE_RT(CPort);
  SET_REQUIRED_TAG(rp->type = scheme_rt_compact_port);
  rp->start = delay_info->cached;
  rp->pos = 0;
  rp->base = 0;
  rp->orig_port = port;
  rp->size = size;
  rp->ut = delay_info->ut;
  rp->unsafe_ok = delay_info->unsafe_ok;
  rp->bytecode_hash = delay_info->bytecode_hash;
  rp->symtab_entries = delay_info->symtab_entries;
  if (delay_info->ut)
    delay_info->ut->rp = rp;

  ht = MALLOC_N(Scheme_Hash_Table *, 1);

  rp->symtab_size = delay_info->symtab_size;
  rp->ht = ht;
  rp->symtab = delay_info->symtab;
  rp->relto = delay_info->relto;
  rp->shared_offsets = delay_info->shared_offsets;
  rp->delay_info = delay_info;
  rp->symtab_refs = scheme_null;

  rp->pos = delay_info->shared_offsets[which - 1];

  if (delay_info->ut)
    prepare_current_unmarshal(delay_info->ut);

  /* Perform the read, catching escapes so we can clean up: */
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  scheme_current_thread->reading_delayed = scheme_true;
  if (scheme_setjmp(newbuf)) {
    v = NULL;
    v_exn = scheme_current_thread->reading_delayed;
  } else {
    v = read_compact(rp, 0);
    v_exn = NULL;
  }
  scheme_current_thread->error_buf = savebuf;
  scheme_current_thread->reading_delayed = NULL;

  /* Clean up: */
  v = resolve_symtab_refs(v, rp);

  delay_info->current_rp = old_rp;
  if (delay_info->ut) {
    delay_info->ut->rp = old_rp;
    complete_current_unmarshal(delay_info->ut);
  }

  if (!old_rp && !delay_info->perma_cache) {
    /* No one using the cache, to register it to be cleaned up */
    delay_info->clear_bytes_next = clear_bytes_chain;
    if (clear_bytes_chain)
      clear_bytes_chain->clear_bytes_prev = delay_info;
    clear_bytes_chain = delay_info;
  }

  scheme_end_atomic_no_swap();
  
  if (v) {
    if (*ht) {
      v = resolve_references(v, port, NULL,
                             scheme_make_hash_table(SCHEME_hash_ptr), 
                             scheme_make_hash_table(SCHEME_hash_ptr),
                             delay_info->symtab_entries,
                             0, 0);
    }

    delay_info->symtab[which] = v;
    record_symtab_self_contained(delay_info->symtab_entries, v);
        
    return v;
  } else {
    if (v_exn && !scheme_current_thread->cjs.is_kill)
      scheme_raise(v_exn);
    scheme_longjmp(*scheme_current_thread->error_buf, 1);
    return NULL;
  }
}

Scheme_Object *scheme_unmarshal_wrap_get(Scheme_Unmarshal_Tables *ut, 
                                         Scheme_Object *wraps_key, 
                                         int *_decoded)
{
  intptr_t l;
  l = SCHEME_INT_VAL(wraps_key);

  if ((l < 0) || ((uintptr_t)l >= ut->rp->symtab_size))
    scheme_ill_formed_code(ut->rp);
  if (SAME_OBJ(ut->rp->symtab[l], SYMTAB_IN_PROGRESS))
    scheme_ill_formed_code(ut->rp);

  if (!ut->rp->symtab[l]) {
    Scheme_Object *v;
    intptr_t save_pos;

    if (!ut->rp->delay_info)
      scheme_ill_formed_code(ut->rp);

    save_pos = ut->rp->pos;
    ut->rp->pos = ut->rp->shared_offsets[l - 1];
    v = read_compact(ut->rp, 0);
    ut->rp->pos = save_pos;
    ut->rp->symtab[l] = v;
  }

  *_decoded = ut->decoded[l];
  return ut->rp->symtab[l];
}

void scheme_unmarshal_wrap_set(Scheme_Unmarshal_Tables *ut, 
                               Scheme_Object *wraps_key, 
                               Scheme_Object *v)
{
  intptr_t l;
  l = SCHEME_INT_VAL(wraps_key);

  ut->rp->symtab[l] = v;
  ut->decoded[l] = 1;
}

/*========================================================================*/
/*                           readtable support                            */
/*========================================================================*/

Scheme_Object *scheme_make_default_readtable()
{
  return scheme_false;
}

static int readtable_kind(Readtable *t, int ch, ReadParams *params)
{
  int k;
  Scheme_Object *v;

  if (ch < 128)
    k = t->fast_mapping[ch];
  else {
    v = scheme_hash_get(t->mapping, scheme_make_integer(ch));
    if (!v) {
      if (scheme_isspace(ch))
	k = READTABLE_WHITESPACE;
      else
	k = READTABLE_CONTINUING;
    } else
      k = SCHEME_INT_VAL(SCHEME_CAR(v));
  }

  if (k == READTABLE_MAPPED) {
    /* ch is mapped to a default behavior: */
    v = scheme_hash_get(t->mapping, scheme_make_integer(ch));
    ch = SCHEME_INT_VAL(SCHEME_CDR(v));
    if (ch < 128)
      k = builtin_fast[ch];
    else if (scheme_isspace(ch))
      k = READTABLE_WHITESPACE;
    else
      k = READTABLE_CONTINUING;
  }

  if (k == READTABLE_MULTIPLE_ESCAPE) {
    /* This is the only one sensitive to params. */
    if (!params->can_read_pipe_quote)
      return READTABLE_CONTINUING;
  }

  return k;
}

static Scheme_Object *readtable_call(int w_char, int ch, Scheme_Object *proc, ReadParams *params,
				     Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
				     int get_info, 
                                     Scheme_Hash_Table **ht, Scheme_Object *modpath_stx)
{
  int cnt, add_srcloc = 0;
  Scheme_Object *a[7], *v;
  Scheme_Cont_Frame_Data cframe;
  
  if (w_char) {
    a[0] = scheme_make_character(ch);
    a[1] = port;
    a[2] = proc;
    if (!src && scheme_check_proc_arity(NULL, 2, 2, 3, a)) {
      cnt = 2;
    } else {
      cnt = 6;
      a[2] = (src ? src : scheme_false);
      add_srcloc = 3;
    }
  } else {
    if (src) {
      a[0] = src;
      a[1] = port;
      if (modpath_stx) {
        a[2] = modpath_stx;
        add_srcloc = 3;
        cnt = 6;
      } else
        cnt = 2;
    } else {
      a[0] = port;
      if (modpath_stx) {
        a[1] = modpath_stx;
        add_srcloc = 2;
        cnt = 5;
      } else
        cnt = 1;
    }
  }

  if (add_srcloc) {
    a[add_srcloc + 0] = (line > 0) ? scheme_make_integer(line) : scheme_false;
    a[add_srcloc + 1] = (col > 0) ? scheme_make_integer(col-1) : scheme_false;
    a[add_srcloc + 2] = (pos > 0) ? scheme_make_integer(pos) : scheme_false;
  }

  if (src) {
    /* fresh ht in case nested uses recursive `read' instead of recursive `read-syntax': */
    ht = MALLOC_N(Scheme_Hash_Table *, 1);
  }

  if (!get_info) {
    scheme_push_continuation_frame(&cframe);
    scheme_set_in_read_mark(src, ht);
  }

  v = scheme_apply(proc, cnt, a);

  if (get_info) {
    a[0] = v;
    if (!scheme_check_proc_arity(NULL, 2, 0, 1, a)) {
      scheme_wrong_contract("read-language", "(any/c any/c . -> . any)", -1, -1, a);
    }
  }

  if (!get_info) {
    scheme_pop_continuation_frame(&cframe);
  }

  if (!get_info && !scheme_special_comment_value(v)) {
    if (SCHEME_STXP(v)) {
      if (!src)
	v = scheme_syntax_to_datum(v, 0, NULL);
    } else if (src) {
      Scheme_Object *s;

      if (*ht) {
        /* resolve references from recursive `read': */
        v = resolve_references(v, port, NULL,
                               scheme_make_hash_table(SCHEME_hash_ptr), 
                               scheme_make_hash_table(SCHEME_hash_ptr),
                               NULL,
                               1, 0);
      }

      s = scheme_make_stx_w_offset(scheme_false, line, col, pos, SPAN(port, pos), src, STX_SRCTAG);
      v = scheme_datum_to_syntax(v, s, scheme_false, 1, 1);
    }

    if (!src)
      set_need_copy(ht);
  }
  
  return v;
}

void scheme_set_in_read_mark(Scheme_Object *src, Scheme_Hash_Table **ht)
{
  Scheme_Object *v;

  if (ht)
    v = scheme_make_raw_pair((Scheme_Object *)ht, 
			     (src ? scheme_true : scheme_false));
  else
    v = scheme_false;
  scheme_set_cont_mark(unresolved_uninterned_symbol, v);
}

static Scheme_Object *readtable_handle(Readtable *t, int *_ch, int *_use_default, ReadParams *params,
				       Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
				       Scheme_Hash_Table **ht)
{
  int ch = *_ch;
  Scheme_Object *v;

  v = scheme_hash_get(t->mapping, scheme_make_integer(ch));

  if (!v) {
    *_use_default = 1;
    return NULL;
  }

  if (SCHEME_INT_VAL(SCHEME_CAR(v)) == READTABLE_MAPPED) {
    *_ch = SCHEME_INT_VAL(SCHEME_CDR(v));
    *_use_default = 1;
    return NULL;
  }

  *_use_default = 0;

  v = SCHEME_CDR(v);

  v = readtable_call(1, ch, v, params, port, src, line, col, pos, 0, ht, NULL);
  
  return v;
}

static int readtable_effective_char(Readtable *t, int ch)
{
  Scheme_Object *v;

  if (!t) return ch;

  v = scheme_hash_get(t->mapping, scheme_make_integer(ch));

  if (v) {
    if (SCHEME_INT_VAL(SCHEME_CAR(v)) == READTABLE_MAPPED)
      return SCHEME_INT_VAL(SCHEME_CDR(v));
    return 0; /* not equivalent to any standard char mapping */
  } else
    return ch;
}

static Scheme_Object *readtable_handle_hash(Readtable *t, int ch, int *_use_default, ReadParams *params,
					    Scheme_Object *port, Scheme_Object *src, intptr_t line, intptr_t col, intptr_t pos,
					    Scheme_Hash_Table **ht)
{
  Scheme_Object *v;

  v = scheme_hash_get(t->mapping, scheme_make_integer(-ch));

  if (!v) {
    *_use_default = 1;
    return NULL;
  }

  *_use_default = 0;

  v = readtable_call(1, ch, v, params, port, src, line, col, pos, 0, ht, NULL);

  if (scheme_special_comment_value(v))
    return NULL;
  else
    return v;
}

static void check_proc_either_arity(const char *who, int a1, int a2, int which, int argc, Scheme_Object **argv)
{
  if (!scheme_check_proc_arity(NULL, a1, which, argc, argv)
      && !scheme_check_proc_arity(NULL, a2, which, argc, argv)) {
    char buffer[256];
    sprintf(buffer, "(or (procedure-arity-includes/c %d) (procedure-arity-includes/c %d))", a1, a2);
    scheme_wrong_contract(who, buffer, which, argc, argv);
  }
}

static Scheme_Object *make_readtable(int argc, Scheme_Object **argv)
{
  Scheme_Object *sym, *val;
  Readtable *t, *orig_t;
  Scheme_Hash_Table *ht;
  char *fast;
  int i, ch;

  if (SCHEME_FALSEP(argv[0]))
    orig_t = NULL;
  else {
    if (!SAME_TYPE(scheme_readtable_type, SCHEME_TYPE(argv[0]))) {
      scheme_wrong_contract("make-readtable", "(or/c readtable? #f)", 0, argc, argv);
      return NULL;
    }
    orig_t = (Readtable *)argv[0];
  }

  t = MALLOC_ONE_TAGGED(Readtable);
  t->so.type = scheme_readtable_type;
  if (orig_t)
    ht = scheme_clone_hash_table(orig_t->mapping);
  else
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
  t->mapping = ht;
  fast = scheme_malloc_atomic(128);
  memcpy(fast, (orig_t ? orig_t->fast_mapping : builtin_fast), 128);
  t->fast_mapping = fast;
  t->symbol_parser = (orig_t ? orig_t->symbol_parser : NULL);

  for (i = 1; i < argc; i += 3) {
    if (!SCHEME_FALSEP(argv[i]) && !SCHEME_CHARP(argv[i])) {
      scheme_wrong_contract("make-readtable", "(or/c char? #f)", i, argc, argv);
      return NULL;
    }

    if (i + 1 >= argc) {
      if (SCHEME_FALSEP(argv[i]))
	scheme_contract_error("make-readtable",
                              "expected 'non-terminating-macro after #f",
                              NULL);
      else
	scheme_contract_error("make-readtable",
                              "expected 'terminating-macro, 'non-terminating-macro, 'dispatch-macro,"
                              " or character argument after character argument",
                              "character", 1, argv[i],
                              NULL);
    }

    sym = argv[i + 1];
    if (!SAME_OBJ(sym, terminating_macro_symbol)
	&& !SAME_OBJ(sym, non_terminating_macro_symbol)
	&& !SAME_OBJ(sym, dispatch_macro_symbol)
	&& !SCHEME_CHARP(sym)) {
      scheme_wrong_contract("make-readtable", 
                            "(or/c 'terminating-macro 'non-terminating-macro 'dispatch-macro char?)", 
                            i+1, argc, argv);
      return NULL;
    }
    if (SCHEME_FALSEP(argv[i])
	&& !SAME_OBJ(sym, non_terminating_macro_symbol)) {
      scheme_contract_error("make-readtable",
                            "expected 'non-terminating-macro after #f",
                            "given", 1, sym,
                            NULL);
    }

    if (i + 2 >= argc) {
      scheme_contract_error("make-readtable",
                            (SCHEME_CHARP(sym) 
                             ? "expected readtable or #f argument after character argument"
                             : "expected procedure argument after symbol argument"),
                            "given", 1, argv[i+1],
                            NULL);
    }

    if (SCHEME_FALSEP(argv[i])) {
      check_proc_either_arity("make-readtable", 6, 7, i+2, argc, argv);
      t->symbol_parser = argv[i + 2];
    } else if (SAME_OBJ(sym, dispatch_macro_symbol)) {
      ch = SCHEME_CHAR_VAL(argv[i]);
      check_proc_either_arity("make-readtable", 6, 7, i+2, argc, argv);
      scheme_hash_set(t->mapping, scheme_make_integer(-ch), argv[i+2]);
    } else {
      if (SCHEME_CHARP(sym)) {
	Readtable *src;
	int sch;

	if (SCHEME_FALSEP(argv[i+2])) {
	  src = NULL;
	} else {
	  if (!SAME_TYPE(scheme_readtable_type, SCHEME_TYPE(argv[i+2]))) {
	    scheme_wrong_contract("make-readtable", "(or/c readtable? #f)", i+2, argc, argv);
	    return NULL;
	  }
	  src = (Readtable *)(argv[i+2]);
	}
	sch = SCHEME_CHAR_VAL(argv[i+1]);
	if (!src)
	  val = NULL; /* use default */
	else
	  val = scheme_hash_get(src->mapping, scheme_make_integer(sch));
	if (!val)
	  val = scheme_make_pair(scheme_make_integer(READTABLE_MAPPED), scheme_make_integer(sch));
      } else {
	int kind;
	check_proc_either_arity("make-readtable", 6, 7, i+2, argc, argv);
	kind = (SAME_OBJ(sym, non_terminating_macro_symbol)
		? READTABLE_CONTINUING
		: READTABLE_TERMINATING);
	val = scheme_make_pair(scheme_make_integer(kind), argv[i+2]);
      }

      ch = SCHEME_CHAR_VAL(argv[i]);
      if (!val) {
	scheme_hash_set(t->mapping, scheme_make_integer(ch), NULL);
	if (ch < 128)
	  t->fast_mapping[ch] = 0;
      } else {
	scheme_hash_set(t->mapping, scheme_make_integer(ch), val);
	if (ch < 128)
	  t->fast_mapping[ch] = (char)SCHEME_INT_VAL(SCHEME_CAR(val));
      }
    }
  }

  return (Scheme_Object *)t;
}

static Scheme_Object *readtable_mapping(int argc, Scheme_Object **argv)
{
  Scheme_Object *v1, *v2, *a[3];
  Readtable *t;
  int ch;

  if (!SAME_TYPE(scheme_readtable_type, SCHEME_TYPE(argv[0]))) {
    scheme_wrong_contract("readtable-mapping", "readtable?", 0, argc, argv);
    return NULL;
  }
  if (!SCHEME_CHARP(argv[1])) {
    scheme_wrong_contract("readtable-mapping", "character?", 1, argc, argv);
    return NULL;
  }
  
  t = (Readtable *)argv[0];
  ch = SCHEME_CHAR_VAL(argv[1]);
  
  v1 = scheme_hash_get(t->mapping, scheme_make_integer(ch));
  v2 = scheme_hash_get(t->mapping, scheme_make_integer(-ch));

  a[0] = argv[1];
  a[1] = scheme_false;
  if (v1) {
    int v;
    v = SCHEME_INT_VAL(SCHEME_CAR(v1));
    if (v & READTABLE_MAPPED) {
      v = SCHEME_INT_VAL(SCHEME_CDR(v1));
      a[0] = scheme_make_character(v);
      a[1] = scheme_false;
    } else if (v & READTABLE_CONTINUING) {
      a[0] = non_terminating_macro_symbol;
      a[1] = SCHEME_CDR(v1);
    } else if (v & READTABLE_TERMINATING) {
      a[0] = terminating_macro_symbol;
      a[1] = SCHEME_CDR(v1);
    }
  }
  a[2] = scheme_false;
  if (v2) {
    a[2] = v2;
  }

  return scheme_values(3, a);
}

static Scheme_Object *readtable_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(scheme_readtable_type, SCHEME_TYPE(argv[0]))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *readtable_or_false_p(int argc, Scheme_Object **argv)
{
  if (SCHEME_FALSEP(argv[0]))
    return scheme_true;
  return readtable_p(argc, argv);
}

static Scheme_Object *current_readtable(int argc, Scheme_Object **argv)
{
  return scheme_param_config2("current-readtable", 
                              scheme_make_integer(MZCONFIG_READTABLE),
                              argc, argv,
                              -1, readtable_or_false_p, "readtable?", 0);
}

static Scheme_Object *current_reader_guard(int argc, Scheme_Object **argv)
{
  return scheme_param_config2("current-reader-guard", 
                              scheme_make_integer(MZCONFIG_READER_GUARD),
                              argc, argv,
                              1, NULL, NULL, 0);
}

static Scheme_Object *no_val_thunk(void *d, int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)d;
}

static Scheme_Object *do_reader(Scheme_Object *try_modpath,
                                Scheme_Object *modpath_stx,
                                Scheme_Object *port,
                                Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
                                int get_info,
                                Scheme_Hash_Table **ht,
                                Scheme_Object *indentation, ReadParams *params)
{
  Scheme_Object *modpath, *name, *a[3], *proc, *v, *no_val;
  int num_a;
  Scheme_Env *env;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;
  int pop_frame;

  if (stxsrc)
    modpath = scheme_syntax_to_datum(modpath_stx, 0, NULL);
  else
    modpath = modpath_stx;

  proc = scheme_get_param(scheme_current_config(), MZCONFIG_READER_GUARD);

  if (try_modpath) {
    a[0] = try_modpath;
    try_modpath = scheme_apply(proc, 1, a);
    
    if (scheme_module_is_declared(try_modpath, 1))
      modpath = try_modpath;
    else
      try_modpath = NULL;
  } 

  if (!try_modpath) {
    a[0] = modpath;
    modpath = scheme_apply(proc, 1, a);
  }
  
  a[0] = modpath;
  if (get_info)
    name = scheme_intern_symbol("get-info");
  else if (stxsrc)
    name = scheme_intern_symbol("read-syntax");
  else
    name = scheme_intern_symbol("read");
  a[1] = name;
  if (get_info) {
    no_val = scheme_make_pair(scheme_false, scheme_false);
    a[2] = scheme_make_closed_prim(no_val_thunk, no_val);
    num_a = 3;
  } else {
    no_val = NULL;
    num_a = 2;
  }

  if (get_info)
    pop_frame = 0;
  else {
    config = scheme_current_config();
    env = scheme_get_env(config);
    
    if (env->reader_env) {
      config = scheme_extend_config(config,
                                    MZCONFIG_ENV,
                                    (Scheme_Object *)env->reader_env);
      scheme_push_continuation_frame(&cframe);
      scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);
      pop_frame = 1;
    } else
      pop_frame = 0;
  }
  
  proc = scheme_dynamic_require(num_a, a);
  if (get_info) {
    proc = scheme_force_value(proc);
  }

  if (get_info && SAME_OBJ(proc, no_val)) {
    v = scheme_false;
  } else {
    a[0] = proc;
    if (scheme_check_proc_arity(NULL, stxsrc ? 6 : 5, 0, 1, a)) {
      /* provide modpath_stx to reader */
    } else if (!get_info && scheme_check_proc_arity(NULL, stxsrc ? 2 : 1, 0, 1, a)) {
      /* don't provide modpath_stx to reader */
      modpath_stx = NULL;
    } else {
      scheme_wrong_contract("#reader",
                            (stxsrc ? "(or/c (any/c any/c . -> . any) (procedure-arity-includes/c 6))"
                             : (get_info
                                ? "(procedure-arity-includes/c 5)"
                                : "(or/c (any/c . -> . any) (procedure-arity-includes/c 5))")),
                            -1, -1, a);
      return NULL;
    }
    
    v = readtable_call(0, 0, proc, params,
                       port, stxsrc, line, col, pos,
                       get_info, ht, modpath_stx);
    
    if (!get_info && scheme_special_comment_value(v))
      v = NULL;
  }

  if (pop_frame)
    scheme_pop_continuation_frame(&cframe);

  return v;
}

/* "#reader" has been read */
static Scheme_Object *read_reader(Scheme_Object *port,
				  Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
				  Scheme_Hash_Table **ht,
				  Scheme_Object *indentation, ReadParams *params)
{
  Scheme_Object *modpath;

  if (stxsrc)
    modpath = scheme_read_syntax(port, stxsrc);
  else
    modpath = scheme_read(port);

  if (SCHEME_EOFP(modpath)) {
    scheme_read_err(port, stxsrc, line, col, pos, 1, EOF, indentation, 
		    "read: expected a datum after #reader, found end-of-file");
    return NULL;
  }

  return do_reader(NULL, modpath, port, stxsrc, line, col, pos, 0, ht, indentation, params);
}

/* "#lang " has been read */
static Scheme_Object *read_lang(Scheme_Object *port,
                                Scheme_Object *stxsrc, intptr_t line, intptr_t col, intptr_t pos,
                                int get_info,
                                Scheme_Hash_Table **ht,
                                Scheme_Object *indentation, ReadParams *params,
                                int init_ch)
{
  int size, len;
  GC_CAN_IGNORE char *sfx;
  char *buf, *naya;
  int ch = 0;
  Scheme_Object *modpath, *subm_modpath;
  intptr_t name_line = -1, name_col = -1, name_pos = -1;

  size = 32;
  buf = MALLOC_N_ATOMIC(char, size);
  len = 0;

  if (init_ch) {
    ch = init_ch;
  } else {
    ch = scheme_getc_special_ok(port);
  }
  scheme_tell_all(port, &name_line, &name_col, &name_pos);

  while (1) {
    /* ch was only peeked at this point (except for the first iteration), so we
       can leave the input immediately after the language spec */
    if (ch == EOF) {
      break;
    } else if (ch == SCHEME_SPECIAL) {
      ch = scheme_getc_special_ok(port);
      scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation,
                      "read: found non-character while reading `#lang'");
    } else if (scheme_isspace(ch)) {
      break;
    } else {
      if (len) ch = scheme_getc_special_ok(port);
      if ((ch < 128)
          && (is_lang_nonsep_char(ch)
              || (ch == '/'))) {
        if (len + 1 >= size) {
          size *= 2;
          naya = MALLOC_N_ATOMIC(char, size);
          memcpy(naya, buf, len * sizeof(char));
          buf = naya;
        }
        buf[len++] = ch;
      } else {
        scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation, 
                        "read: expected only alphanumeric, `-', `+', `_', or `/'"
                        " characters for `#%s', found %c",
                        init_ch ? "!" : "lang",
                        ch);
        return NULL;
      }
    }
    ch = scheme_peekc_special_ok(port);
  }

  if (!len) {
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation, 
                    (((ch == ' ') && !init_ch)
                     ? "read: expected a single space after `#lang'"
                     : "read: expected a non-empty sequence of alphanumeric, `-', `+', `_', or `/' after `#%s'"),
                    init_ch ? "!" : "lang ");
    return NULL;
  }
  if (buf[0] == '/') {
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation, 
                    "read: expected a name that does not start `/' after `#lang'");
    return NULL;
  }
  if (buf[len - 1] == '/') {
    scheme_read_err(port, stxsrc, line, col, pos, SPAN(port, pos), ch, indentation, 
                    "read: expected a name that does not end `/' after `#%s'",
                    init_ch ? "!" : "lang");
    return NULL;
  }

  if (len + 16 >= size) {
    size += 16;
    naya = MALLOC_N_ATOMIC(char, size * sizeof(char));
    memcpy(naya, buf, len * sizeof(char));
    buf = naya;
  }
  buf[len] = 0;
  subm_modpath = scheme_intern_symbol(buf);

  sfx = "/lang/reader";
  while (*sfx) {
    buf[len++] = *(sfx++);
  }
  buf[len] = 0;

  modpath = scheme_intern_symbol(buf);
  if (stxsrc) {
    intptr_t span;
    span = SPAN(port, name_pos);
    modpath = scheme_make_stx_w_offset(modpath, name_line, name_col, name_pos, 
                                       span, 
                                       stxsrc, STX_SRCTAG);
  }

  subm_modpath = scheme_make_pair(scheme_intern_symbol("submod"),
                                  scheme_make_pair(subm_modpath,
                                                   scheme_make_pair(scheme_intern_symbol("reader"),
                                                                    scheme_null)));

  return do_reader(subm_modpath, modpath, port, stxsrc, line, col, pos, get_info, ht, indentation, params);
}

Scheme_Object *scheme_read_language(Scheme_Object *port, int nonlang_ok)
{
  return _internal_read(port, NULL, 0, 0, 0, 0, -1,
                        NULL, NULL, NULL, NULL, nonlang_ok ? 2 : 1);
}

static Scheme_Object *expected_lang(const char *prefix, int ch,
                                    Scheme_Object *port, Scheme_Object *stxsrc,
                                    intptr_t line, intptr_t col, intptr_t pos,
                                    int get_lang)
{
  if (get_lang > 1) {
    return scheme_void;
  } else {
    mzchar chs[2];
    char *more;

    chs[0] = 0;
    chs[1] = 0;
      
    if (ch == EOF)
      more = "an end-of-file";
    else if (ch == SCHEME_SPECIAL)
      more = "a non-character";
    else {
      chs[0] = ch;
      more = "";
    }

    scheme_read_err(port, stxsrc, line, col, pos, 1, ch, NULL, 
                    "read-language: expected (after whitespace and comments)"
                    " `#lang ' or `#!' followed"
                    " immediately by a language name, found %s%s%5%s%s%s",
                    (*prefix || *chs) ? "`" : "", 
                    prefix, chs, 
                    (*prefix || *chs) ? "`" : "", 
                    ((*prefix || *chs) && *more) ? " followed by " : "", 
                    more);
    
    return NULL;
  }
}

/*========================================================================*/
/*                         precise GC traversers                          g*/
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_read.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_indent_type, mark_indent);
  GC_REG_TRAV(scheme_rt_compact_port, mark_cport);
  GC_REG_TRAV(scheme_readtable_type, mark_readtable);
  GC_REG_TRAV(scheme_rt_read_params, mark_read_params);
  GC_REG_TRAV(scheme_rt_delay_load_info, mark_delay_load);
  GC_REG_TRAV(scheme_rt_unmarshal_info, mark_unmarshal_tables);
}

END_XFORM_SKIP;

#endif
