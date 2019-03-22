/* This file contains a restructed Racket reader for reading startup
   code and for ".zo" files. The normal reader is a recursive-descent
   parser. The really messy part is number parsing, which is in a
   different file, "numstr.c".

   Rule on using scheme_ungetc(): the reader is generally allowed to
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
SHARED_OK static int validate_loaded_linklet = 0;

THREAD_LOCAL_DECL(int scheme_num_read_syntax_objects = 0);

/* read-only global symbols */
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
ROSYM static Scheme_Object *hash_code_symbol;
ROSYM static Scheme_Object *pre_symbol;
ROSYM static Scheme_Object *post_symbol;

/* local function prototypes */
static Scheme_Object *read_case_sensitive(int, Scheme_Object *[]);
static Scheme_Object *read_accept_pipe_quote(int, Scheme_Object *[]);
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

#define isdigit_ascii(n) ((n >= '0') && (n <= '9'))

#define scheme_isxdigit(n) (isdigit_ascii(n) || ((n >= 'a') && (n <= 'f')) || ((n >= 'A') && (n <= 'F')))

#define mz_shape_cons 0
#define mz_shape_vec 1
#define mz_shape_hash_list 2
#define mz_shape_hash_elem 3
#define mz_shape_vec_plus_infix 4
#define mz_shape_fl_vec 5
#define mz_shape_fx_vec 6

#define MAX_GRAPH_ID_DIGITS 8

typedef struct ReadParams {
  MZTAG_IF_REQUIRED
  char skip_zo_vers_check;
  char can_read_unsafe;
  Scheme_Object *delay_load_info;
  Scheme_Object *read_relative_path;
  Scheme_Hash_Table *graph_ht;
} ReadParams;

#define THREAD_FOR_LOCALS scheme_current_thread

static Scheme_Object *read_list(Scheme_Object *port,
				int opener, int closer,
				int shape, int use_stack,
				ReadParams *params);
static Scheme_Object *read_string(int is_byte,
				  Scheme_Object *port,
				  ReadParams *params,
                                  int err_ok);
static Scheme_Object *read_quote(char *who, Scheme_Object *quote_symbol, int len,
				 Scheme_Object *port,
				 ReadParams *params);
static Scheme_Object *read_vector(Scheme_Object *port,
				  int opener, char closer,
				  ReadParams *params,
                                  int allow_infix);
static Scheme_Object *read_number_or_symbol(int init_ch, Scheme_Object *port,
                                            int is_float, int is_not_float,
                                            int radix, int radix_set,
                                            int is_symbol, int is_kw,
                                            ReadParams *params);
static Scheme_Object *read_number(int init_ch,
				  Scheme_Object *port,
				  int, int, int, int,
				  ReadParams *params);
static Scheme_Object *read_symbol(int init_ch,
				  Scheme_Object *port,
				  ReadParams *params);
static Scheme_Object *read_keyword(int init_ch,
				   Scheme_Object *port,
				   ReadParams *params);
static Scheme_Object  *read_delimited_constant(int ch, const mzchar *str,
                                               Scheme_Object *v,
                                               Scheme_Object *port,
                                               ReadParams *params);
static Scheme_Object *read_character(Scheme_Object *port,
				     ReadParams *params);
static Scheme_Object *read_box(Scheme_Object *port,
			       ReadParams *params);
static Scheme_Object *read_hash(Scheme_Object *port,
				int opener, char closer, int kind,
				ReadParams *params);
static void unexpected_closer(int ch,
			      Scheme_Object *port);
static int next_is_delim(Scheme_Object *port);
static int read_graph_index(Scheme_Object *port, int *ch);
static int skip_whitespace_comments(Scheme_Object *port,
				    ReadParams *params);

static Scheme_Object *read_intern(int argc, Scheme_Object **argv);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define SCHEME_OK          0x1

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

void scheme_init_read(Scheme_Startup_Env *env)
{
  REGISTER_SO(quote_symbol);
  REGISTER_SO(quasiquote_symbol);
  REGISTER_SO(unquote_symbol);
  REGISTER_SO(unquote_splicing_symbol);
  REGISTER_SO(syntax_symbol);
  REGISTER_SO(unsyntax_symbol);
  REGISTER_SO(unsyntax_splicing_symbol);
  REGISTER_SO(quasisyntax_symbol);

  REGISTER_SO(hash_code_symbol);
  REGISTER_SO(pre_symbol);
  REGISTER_SO(post_symbol);

  quote_symbol                  = scheme_intern_symbol("quote");
  quasiquote_symbol             = scheme_intern_symbol("quasiquote");
  unquote_symbol                = scheme_intern_symbol("unquote");
  unquote_splicing_symbol       = scheme_intern_symbol("unquote-splicing");
  syntax_symbol                 = scheme_intern_symbol("syntax");
  unsyntax_symbol               = scheme_intern_symbol("unsyntax");
  unsyntax_splicing_symbol      = scheme_intern_symbol("unsyntax-splicing");
  quasisyntax_symbol            = scheme_intern_symbol("quasisyntax");

  hash_code_symbol             = scheme_intern_symbol("hash-code");
  pre_symbol                   = scheme_intern_symbol("pre");
  post_symbol                  = scheme_intern_symbol("post");

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

  ADD_PARAMETER("read-case-sensitive",           read_case_sensitive,    MZCONFIG_CASE_SENS,                   env);
  ADD_PARAMETER("read-accept-bar-quote",         read_accept_pipe_quote, MZCONFIG_CAN_READ_PIPE_QUOTE,         env);
#ifdef LOAD_ON_DEMAND
  ADD_PARAMETER("read-on-demand-source",         read_delay_load,        MZCONFIG_DELAY_LOAD_INFO,             env);
#endif
  ADD_PARAMETER("print-graph",                   print_graph,            MZCONFIG_PRINT_GRAPH,                 env);
  ADD_PARAMETER("print-struct",                  print_struct,           MZCONFIG_PRINT_STRUCT,                env);
  ADD_PARAMETER("print-box",                     print_box,              MZCONFIG_PRINT_BOX,                   env);
  ADD_PARAMETER("print-vector-length",           print_vec_shorthand,    MZCONFIG_PRINT_VEC_SHORTHAND,         env);
  ADD_PARAMETER("print-hash-table",              print_hash_table,       MZCONFIG_PRINT_HASH_TABLE,            env);
  ADD_PARAMETER("print-unreadable",              print_unreadable,       MZCONFIG_PRINT_UNREADABLE,            env);
  ADD_PARAMETER("print-pair-curly-braces",       print_pair_curly,       MZCONFIG_PRINT_PAIR_CURLY,            env);
  ADD_PARAMETER("print-mpair-curly-braces",      print_mpair_curly,      MZCONFIG_PRINT_MPAIR_CURLY,           env);
  ADD_PARAMETER("print-syntax-width",            print_syntax_width,     MZCONFIG_PRINT_SYNTAX_WIDTH,          env);
  ADD_PARAMETER("print-reader-abbreviations",    print_reader,           MZCONFIG_PRINT_READER,                env);
  ADD_PARAMETER("print-boolean-long-form",       print_long_bool,        MZCONFIG_PRINT_LONG_BOOLEAN,          env);
  ADD_PARAMETER("print-as-expression",           print_as_qq,            MZCONFIG_PRINT_AS_QQ,                 env);

  ADD_NONCM_PRIM("datum-intern-literal", read_intern, 1, 1, env);

  if (getenv("PLT_DELAY_FROM_ZO"))
    use_perma_cache = 0;
  if (getenv("PLT_VALIDATE_LOAD"))
    validate_loaded_linklet = 0;
}

void scheme_init_variable_references_constants()
{
  REGISTER_SO(variable_references);
  variable_references = scheme_make_builtin_references_table(&unsafe_variable_references_start);
}

Scheme_Object *scheme_position_to_builtin(int l)
{
  if (l < EXPECTED_PRIM_COUNT)
    return variable_references[l];
  else
    return NULL;
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
read_accept_pipe_quote(int argc, Scheme_Object *argv[])
{
  DO_CHAR_PARAM("read-accept-pipe-quote", MZCONFIG_CAN_READ_PIPE_QUOTE);
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

static Scheme_Object *read_inner(Scheme_Object *port, ReadParams *params, int pre_char);

static Scheme_Object *read_inner_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  ReadParams *params = (ReadParams *)SCHEME_CDR((Scheme_Object *)p->ku.k.p4);

  p->ku.k.p1 = NULL;
  p->ku.k.p4 = NULL;

  return read_inner(o, params, p->ku.k.i2);
}

static Scheme_Object *read_inner(Scheme_Object *port, ReadParams *params, int pre_char)
{
  int ch, ch2;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      ReadParams *params2;

      /* params may be on the stack, so move it to the heap: */
      params2 = MALLOC_ONE_RT(ReadParams);
      memcpy(params2, params, sizeof(ReadParams));
#ifdef MZ_PRECISE_GC
      params2->type = scheme_rt_read_params;
#endif

      p->ku.k.p1 = (void *)port;
      p->ku.k.p4 = (void *)params2;
      p->ku.k.i2 = pre_char;
      return scheme_handle_stack_overflow(read_inner_k);
    }
  }
#endif

 start_over:

  SCHEME_USE_FUEL(1);

  /* Skip whitespace */
  while (1) {
    if (pre_char >= 0) {
      ch = pre_char;
      pre_char = -1;
    } else
      ch = scheme_getc(port);
    if (NOT_EOF_OR_SPECIAL(ch)) {
      if (!scheme_isspace(ch))
	break;
    } else
      break;
  }

  switch (ch)
    {
    case EOF: 
      return scheme_eof;
    case ']':
      unexpected_closer(ch, port);
      return NULL;
    case '}':
      unexpected_closer(ch, port);
      return NULL;
    case ')':
      unexpected_closer(ch, port);
      return NULL;
    case '(':
      return read_list(port, ch, ')', mz_shape_cons, 0, params);
    case '[':
      return read_list(port, ch, ']', mz_shape_cons, 0, params);
    case '{':
      return read_list(port, ch, '}', mz_shape_cons, 0, params);
    case '|':
      return read_symbol(ch, port, params);
    case '"':
      return read_string(0, port, params, 1);
    case '\'':
      return read_quote("quoting '", quote_symbol, 1, port, params);
    case '`':
      return read_quote("quasiquoting `", quasiquote_symbol, 1, port, params);
    case ',':
      {
	if (scheme_peekc(port) == '@') {
	  ch = scheme_getc(port); /* must be '@' */
	  return read_quote("unquoting ,@", unquote_splicing_symbol, 2, port, params);
	} else
	  return read_quote("unquoting ,", unquote_symbol, 1, port, params);
      }
    case ';':
      {
	while (((ch = scheme_getc(port)) != '\n') 
               && !is_line_comment_end(ch)) {
	  if (ch == EOF)
            return scheme_eof;
	}
	goto start_over;
      }
    case '#':
      ch = scheme_getc(port);

      switch (ch)
	{
	case EOF:
	  scheme_read_err(port, "read: bad syntax `#'");
          return NULL;
	case ';':
	  {
	    Scheme_Object *skipped;
	    skipped = read_inner(port, params, -1);
	    if (SCHEME_EOFP(skipped))
	      scheme_read_err(port, "read: expected a commented-out element for `#;' (found end-of-file)");
	    goto start_over;
	  }
	case '%':
          scheme_ungetc('%', port);
          return read_symbol('#', port, params);
	case ':':
          return read_keyword(-1, port, params);
	case '(':
          return read_vector(port, ch, ')', params, 0);
	case '[':
          return read_vector(port, ch, ']', params, 0);
	case '{':
          return read_vector(port, ch, '}', params, 0);
	case '\\':
	  return read_character(port, params);
	case 'T':
	case 't': 
          if (next_is_delim(port)) {
            /* found delimited `#t' */
            return scheme_true;
          } else {
            GC_CAN_IGNORE const mzchar str[] = { 't', 'r', 'u', 'e', 0 };
            return read_delimited_constant(ch, str, scheme_true, port, params);
          }
	case 'F':
	case 'f': 
          if (next_is_delim(port)) {
            /* found delimited `#f' */
            return scheme_false;
          } else {
            GC_CAN_IGNORE const mzchar str[] = { 'f', 'a', 'l', 's', 'e', 0 };
            return read_delimited_constant(ch, str, scheme_false, port, params);
          }
	case 's':
	case 'S':
          {
            int orig_ch = ch;
            ch = scheme_getc(port);
            if ((orig_ch == 's') 
                && ((ch == '(')
                    || (ch == '[')
                    || (ch == '{'))) {
              Scheme_Object *v;
              Scheme_Struct_Type *st;
              
              if (ch == '(')
                ch = ')';
              else if (ch == '[')
                ch = ']';
              else if (ch == '{')
                ch = '}';

              v = read_vector(port, orig_ch, ch, params, 1);
              
              if (SCHEME_VEC_SIZE(v))
                st = scheme_lookup_prefab_type(SCHEME_VEC_ELS(v)[0], SCHEME_VEC_SIZE(v) - 1);
              else
                st = NULL;

              if (!st || (st->num_slots != (SCHEME_VEC_SIZE(v) - 1))) {
                scheme_read_err(port,
                                (SCHEME_VEC_SIZE(v)
                                 ? (st
                                    ? ("read: mismatch between structure description"
                                       " and number of provided field values in `#s' form")
                                    : "read: invalid structure description in `#s' form")
                                 : "read: missing structure description in `#s' form"));
                return NULL;
              }

              v = scheme_make_prefab_struct_instance(st, v);

              return v;
            } else {
              scheme_read_err(port,
                              "read: expected `x'%s after `#%c'",
                              (orig_ch == 's' ? "or `('" : ""),
                              orig_ch);
              return NULL;
            }
          }
	case 'X':
	case 'x': 
          return read_number(-1, port, 0, 0, 16, 1, params);
	case 'B':
	case 'b': 
          return read_number(-1, port, 0, 0, 2, 1, params);
	case 'O':
	case 'o': 
          return read_number(-1, port, 0, 0, 8, 1, params);
	case 'D':
	case 'd': 
          return read_number(-1, port, 0, 0, 10, 1, params);
	case 'E':
	case 'e': 
          return read_number(-1, port, 0, 1, 10, 0, params);
	case 'I':
	case 'i': 
          return read_number(-1, port, 1, 0, 10, 0, params);
	case '\'':
          return read_quote("quoting #'", syntax_symbol, 2, port, params);
	case '`':
          return read_quote("quasiquoting #`", quasisyntax_symbol, 2, port, params);
	case ',':
          if (scheme_peekc(port) == '@') {
            ch = scheme_getc(port); /* must be '@' */
            return read_quote("unquoting #`@", unsyntax_splicing_symbol, 3, port, params);
          } else
            return read_quote("unquoting #`", unsyntax_symbol, 2, port, params);
        case '^':
          {
            ch = scheme_getc(port);
            if (ch == '#') {
              ch = scheme_getc(port);
              if (ch == '"') {
                Scheme_Object *str;

                str = read_string(1, port, params, 1);

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
              } else {
                scheme_read_err(port, "read: bad syntax `#^#%c'", ch);
                return NULL;
              }
            } else {
              scheme_read_err(port, "read: bad syntax `#^%c'", ch);
              return NULL;
            }
          }
          break;
	case '|':
	  {
	    intptr_t depth = 0;
	    ch2 = 0;
	    do {
	      ch = scheme_getc(port);

	      if (ch == EOF)
		scheme_read_err(port, "read: end of file in #| comment");

	      if ((ch2 == '|') && (ch == '#')) {
		if (!(depth--))
		  goto start_over;
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
          return read_box(port, params);
	case 'r':
	case 'p':
	  {
	    int orig_ch = ch;
	    int cnt = 0, is_byte = 0;
	    char *expect;

	    ch = scheme_getc(port);
	    if (ch == 'x') {
	      expect = "x#";
	      ch = scheme_getc(port);
	      cnt++;
	      if (ch == '#') {
		is_byte = 1;
		cnt++;
		ch = scheme_getc(port);
	      }
	      if (ch == '"') {
		Scheme_Object *str;
		int is_err;

		str = read_string(is_byte, port, params, 1);

		str = scheme_make_regexp(str, is_byte, (orig_ch == 'p'), &is_err);

		if (is_err) {
		  scheme_read_err(port, "read: bad %sregexp string `%s`", 
				  (orig_ch == 'r') ? "" : "p",
				  (char *)str);
		  return NULL;
		}

		return str;
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

	      scheme_read_err(port, "read: bad syntax `#%c%u`",
			      orig_ch, a, (intptr_t)cnt);
	      return NULL;
	    }
	  }
	  break;
	case 'h':
	  {
	    ch = scheme_getc(port);
	    if (ch != 'a') {
              scheme_read_err(port, "read: expected `a` after `#h`");
              return NULL;
	    } else {
	      GC_CAN_IGNORE const mzchar str[] = { 's', 'h', 'e', 'q', 'v', 0 };
	      int scanpos = 0, failed = 0;

	      do {
		ch = scheme_getc(port);
		if ((mzchar)ch == str[scanpos]) {
		  scanpos++;
		} else {
		  if ((scanpos == 2) || (scanpos == 4)) {
		    if (!(ch == '(')
			&& !(ch == '[')
			&& !(ch == '{'))
		      failed = 1;
		  } else
		    failed = 1;
		  break;
		}
	      } while (str[scanpos]);
              
	      if (!failed) {
		/* Found recognized tag. Look for open paren... */
                int kind;

		if (scanpos > 4)
		  ch = scheme_getc(port);
                
                if (scanpos == 4)
                  kind = 0;
                else if (scanpos == 2)
                  kind = 1;
                else 
                  kind = 2;

		if (ch == '(')
		  return read_hash(port, ch, ')', kind, params);
		if (ch == '[')
		  return read_hash(port, ch, ']', kind, params);
		if (ch == '{')
		  return read_hash(port, ch, '}', kind, params);
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

		scheme_read_err(port, "read: bad syntax `#ha%5%u'",
				str_part,
				one_more, (intptr_t)(NOT_EOF_OR_SPECIAL(ch) ? 1 : 0));
		return NULL;
	      }
	    }
	  }
	  break;
	case '"':
          return read_string(1, port, params, 1);
	default:
          if (ch == '(')
            return read_vector(port, ch, ')', params, 0);
          else if (ch == '[')
            return read_vector(port, ch, ']', params, 0);
          else if (ch == '{')
            return read_vector(port, ch, '}', params, 0);
          else if (isdigit_ascii(ch)) {
            /* graph definition or reference */
            int nch = ch, index;
            Scheme_Object *val;

            index = read_graph_index(port, &nch);
            switch (nch) {
            case '#':
              if (params->graph_ht)
                val = scheme_hash_get(params->graph_ht, scheme_make_integer(index));
              else
                val = NULL;
              if (!val)
                scheme_read_err(port,
                                "read: no value for `#%d#`",
                                index);
              return val;
            case '=':
              if (!params->graph_ht) {
                Scheme_Hash_Table *ht;
                ht = scheme_make_hash_table(SCHEME_hash_ptr);
                params->graph_ht = ht;
              }
              if (scheme_hash_get(params->graph_ht, scheme_make_integer(index)))
                scheme_read_err(port,
                                "read: duplicate `#%d=` definition",
                                index);
              val = read_inner(port, params, -1);
              scheme_hash_set(params->graph_ht, scheme_make_integer(index), val);
              return val;
            default:
              scheme_read_err(port,
                              "read: expected `=` or `#` after `#%d`, found `%c`",
                              index, nch);
              return NULL;
            }
            
          } else {
            scheme_read_err(port, "read: bad syntax `#%c`", ch);
            return NULL;
          }
	}
    default:
      return read_number_or_symbol(ch, port, 0, 0, 10, 0, 0, 0, params);
    }
}

#ifdef DO_STACK_CHECK
static Scheme_Object *resolve_references(Scheme_Object *obj,
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
  Scheme_Object *top = (Scheme_Object *)p->ku.k.p5;
  Scheme_Hash_Table *dht = (Scheme_Hash_Table *)p->ku.k.p3;
  Scheme_Hash_Table *tht = (Scheme_Hash_Table *)SCHEME_CAR((Scheme_Object *)p->ku.k.p4);
  Scheme_Hash_Table *self_contained_ht = (Scheme_Hash_Table *)SCHEME_CDR((Scheme_Object *)p->ku.k.p4);

  p->ku.k.p1 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  return resolve_references(o, top, dht, tht, self_contained_ht, p->ku.k.i1, p->ku.k.i2);
}
#endif

static Scheme_Object *resolve_references(Scheme_Object *obj,
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
        scheme_contract_error("make-reader-graph",
                              "illegal placeholder cycle in value",
                              "value", 1, top,
                              NULL);
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

    rr = resolve_references(SCHEME_CAR(obj), top, dht, tht, self_contained_ht,
                            clone, tail_depth + 1);
    SCHEME_CAR(result) = rr;

    scheme_hash_set(tht, result, scheme_make_integer(tail_depth));

    rr = resolve_references(SCHEME_CDR(obj), top, dht, tht, self_contained_ht,
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

    rr = resolve_references(SCHEME_BOX_VAL(obj), top, dht, tht, self_contained_ht,
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
	rr = resolve_references(prev_v, top, dht, tht, self_contained_ht,
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

    lst = resolve_references(lst, top, dht, tht, self_contained_ht,
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
    l = resolve_references(l, top, dht, tht, self_contained_ht,
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
	v = resolve_references(prev_v, top, dht, tht, self_contained_ht,
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
_internal_read(Scheme_Object *port, int crc, int cant_fail,
               int extra_char,
               Scheme_Object *delay_load_info)
{
  Scheme_Object *v, *v2;
  ReadParams params;

  if (crc >= 0) {
    params.can_read_unsafe = 1;
  } else {
    v = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
    v2 = scheme_get_initial_inspector();
    params.can_read_unsafe = SAME_OBJ(v, v2);
  }
  params.read_relative_path = NULL;
  if (!delay_load_info)
    delay_load_info = scheme_get_param(scheme_current_config(), MZCONFIG_DELAY_LOAD_INFO);
  if (SCHEME_TRUEP(delay_load_info))
    params.delay_load_info = delay_load_info;
  else
    params.delay_load_info = NULL;
  params.skip_zo_vers_check = cant_fail;
  params.graph_ht = NULL;

  v = read_inner(port, &params, extra_char);
  
  if (params.graph_ht)
    v = resolve_references(v, NULL,
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           NULL, 0, 0);

  return v;
}

static void *scheme_internal_read_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *delay_load_info = (Scheme_Object *)p->ku.k.p5;

  p->ku.k.p1 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  return (void *)_internal_read(port, p->ku.k.i1, 0, p->ku.k.i4, delay_load_info);
}

Scheme_Object *
scheme_internal_read(Scheme_Object *port, int crc, int cantfail,
		     int pre_char,
                     Scheme_Object *delay_load_info)
{
  Scheme_Thread *p = scheme_current_thread;

  if (cantfail) {
    return _internal_read(port, crc, cantfail, -1, delay_load_info);
  } else {
    p->ku.k.p1 = (void *)port;
    p->ku.k.i1 = crc;
    p->ku.k.i4 = pre_char;
    p->ku.k.p5 = (void *)delay_load_info;

    return (Scheme_Object *)scheme_top_level_do(scheme_internal_read_k, 0);
  }
}

Scheme_Object *scheme_read(Scheme_Object *port)
{
  Scheme_Object *read_proc, *a[1];
  read_proc = scheme_get_startup_export("read");
  a[0] = port;
  return scheme_apply(read_proc, 1, a);
}

Scheme_Object *scheme_read_syntax(Scheme_Object *port, Scheme_Object *stxsrc)
{
  Scheme_Object *read_syntax_proc, *a[2];
  read_syntax_proc = scheme_get_startup_export("read-syntax");
  a[0] = stxsrc;
  a[1] = port;
  return scheme_apply(read_syntax_proc, 2, a);
}

Scheme_Object *scheme_resolve_placeholders(Scheme_Object *obj)
{
  return resolve_references(obj, obj, 
                            scheme_make_hash_table(SCHEME_hash_ptr),
                            scheme_make_hash_table(SCHEME_hash_ptr),
                            NULL,
                            1, 0);
}

/*========================================================================*/
/*                             list reader                                */
/*========================================================================*/

static int next_is_delim(Scheme_Object *port)
{
  int next;
  next = scheme_peekc(port);
  return ((next == EOF)
	  || (next == SCHEME_SPECIAL)
	  || (scheme_isspace(next)
              || (next == '(')
              || (next == ')')
              || (next == '"')
              || (next == ';')
              || (next == '\'')
              || (next == '`')
              || (next == ',')
              || ((next == '['))
              || ((next == '{'))
              || ((next == ']'))
              || ((next == '}'))));
}

/* "(" (or other opener) has already been read */
static Scheme_Object *
read_list(Scheme_Object *port,
	  int opener, int closer, int shape, int use_stack,
	  ReadParams *params)
{
  Scheme_Object *list = NULL, *last = NULL, *car, *cdr, *pair, *infixed = NULL;
  int ch = 0, got_ch_already = 0;

  while (1) {
    if (got_ch_already)
      got_ch_already = 0;
    else
      ch = skip_whitespace_comments(port, params);

    if ((ch == EOF) && (closer != EOF)) {
      scheme_read_err(port, "read: expected a `%c` to close `%c`", closer, opener);
      return NULL;
    }

    if (ch == closer) {
      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, "read: expected hash pair (with key and value separated by `.`) before `%c`", ch);
	return NULL;
      }
      if (!list) list = scheme_null;
      return list;
    }

    if (shape == mz_shape_hash_list) {
      /* Make sure we found a parenthesized something. */
      if (!(ch == '(')
	  && !(ch == '[')
	  && !(ch == '{')) {
        scheme_read_err(port, "read: expected `(`, `[`, or `{` to start a hash pair");
	return NULL;
      } else {
	/* Found paren. Use read_list directly so we can specify mz_shape_hash_elem. */
	car = read_list(port,
			ch, ((ch == '(') ? ')' : ((ch == '[') ? ']' : '}')),
			mz_shape_hash_elem, use_stack, params);
	/* car is guaranteed to have an appropriate shape */
      }
    } else {
      car = read_inner(port, params, ch);
      /* can't be eof, due to check above */
    }

    pair = scheme_make_pair(car, scheme_null);

    ch = skip_whitespace_comments(port, params);
    if (ch == closer) {
      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, "read: expected `.` and value for hash before `%c`", ch);
	return NULL;
      }

      cdr = pair;
      if (!list)
	list = cdr;
      else
	SCHEME_CDR(last) = cdr;

      if (infixed) {
	list = scheme_make_pair(infixed, list);
      }
      return list;
    } else if ((ch == '.')
	       && next_is_delim(port)) {
      if (((shape != mz_shape_cons) 
           && (shape != mz_shape_hash_elem)
           && (shape != mz_shape_vec_plus_infix))
          || infixed) {
	scheme_read_err(port, "read: illegal use of `.`");
	return NULL;
      }

      /* can't be eof, due to check above: */
      cdr = read_inner(port, params, -1);
      ch = skip_whitespace_comments(port, params);
      if ((ch != closer) || (shape == mz_shape_vec_plus_infix)) {
	if ((ch == '.') 
            && next_is_delim(port)) {
	  /* Parse as infix: */

	  if (shape == mz_shape_hash_elem) {
	    scheme_read_err(port, "read: expected `%c` after hash value", closer);
	    return NULL;
	  }

	  infixed = cdr;

	  if (!list)
	    list = pair;
	  else
	    SCHEME_CDR(last) = pair;
	  last = pair;

	  /* Make sure there's not a closing paren immediately after the dot: */
	  ch = skip_whitespace_comments(port, params);
	  if ((ch == closer) || (ch == EOF)) {
	    scheme_read_err(port, "read: illegal use of `%c`", ch);
	    return NULL;
	  }
          got_ch_already = 1;
	} else {
	  scheme_read_err(port, "read: illegal use of `.`");
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
	return list;
      }
    } else {
      got_ch_already = 1;

      if (shape == mz_shape_hash_elem) {
	scheme_read_err(port, "read: expected `.` and value for hash");
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

/*========================================================================*/
/*                            string reader                               */
/*========================================================================*/

/* '"' has already been read */
static Scheme_Object *
read_string(int is_byte, Scheme_Object *port, ReadParams *params, int err_ok)
{
  mzchar *buf, *oldbuf, onstack[32];
  int i, j, n, n1, ch, closer = '"';
  intptr_t size = 31, oldsize;
  Scheme_Object *result;

  i = 0;
  buf = onstack;
  while (1) {
    ch = scheme_getc(port);
    if (ch == closer)
      break;

    if (ch == EOF) {
      if (err_ok)
	scheme_read_err(port, "read: expected a closing %s%s",
			"'\"'",
			(ch == EOF) ? "" : " after one character");
      return NULL;
    }
    /* Note: errors will tend to leave junk on the port, with an open \". */
    /* Escape-sequence handling by Eli Barzilay. */
    if (ch == '\\') {
      ch = scheme_getc(port);
      if (ch == EOF) {
	if (err_ok)
	  scheme_read_err(port, "read: expected a closing %s", "'\"'");
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
        if (scheme_peekc(port) == '\n')
	  scheme_getc(port);
	continue; /* <---------- !!!! */
      case '\n':
        continue; /* <---------- !!!! */
      case 'x':
	ch = scheme_getc(port);
	if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	  n = ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10);
	  ch = scheme_peekc(port);
	  if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	    n = n*16 + (ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10));
	    scheme_getc(port); /* must be ch */
	  }
	  ch = n;
	} else {
	  if (err_ok)
	    scheme_read_err(port, "read: no hex digit following \\x in string");
	  return NULL;
	}
	break;
      case 'u':
      case 'U':
	if (!is_byte) {
	  int maxc = ((ch == 'u') ? 4 : 8);
          char initial[9];
	  ch = scheme_getc(port);
	  if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
	    int count = 1;
            initial[0] = ch;
	    n = ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10);
	    while (count < maxc) {
	      ch = scheme_peekc(port);
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
              ch = scheme_getc(port);
              if (ch == '\\') {
                snd[sndp++] = ch;
                ch = scheme_getc(port);
                if (ch == 'u') {
                  snd[sndp++] = ch;
                  ch = scheme_getc(port);
                  if ((ch == 'd') || (ch == 'D')) {
                    snd[sndp++] = ch;
                    ch = scheme_getc(port);
                    if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                      snd[sndp++] = ch;
                      n2 = (scheme_toupper(ch)-'A'+10);
                      if ((n2 >= 12) && (n2 <= 15)) {
                        n2 = 0xD000 | (n2 << 8);
                        ch = scheme_getc(port);
                        if (NOT_EOF_OR_SPECIAL(ch) && scheme_isxdigit(ch)) {
                          snd[sndp++] = ch;
                          n2 |= ((ch<='9' ? ch-'0' : (scheme_toupper(ch)-'A'+10)) << 4);
                          ch = scheme_getc(port);
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
                if (NOT_EOF_OR_SPECIAL(ch))
                  snd[sndp++] = ch;
                snd[sndp] = 0;
                if (err_ok)
                  scheme_read_err(port,
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
	    if (err_ok)
	      scheme_read_err(port,
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
		scheme_read_err(port,
				"read: escape sequence \\%o out of range in %s", n1,
				"string");
	      return NULL;
	    }
	    n = n1;
	    if (j < 2) {
	      ch = scheme_peekc(port);
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
	    scheme_read_err(port,
			    "read: unknown escape sequence \\%c in %s%s", ch,
			    is_byte ? "byte " : "",
			    "string");
	  return NULL;
	}
	break;
      }
    } else if (is_byte && (ch > 255)) {
      if (err_ok)
	scheme_read_err(port,
			"read: out-of-range character in byte string: %c",
                        ch);
      return NULL;
    }

    if (ch < 0) {
      if (err_ok)
	scheme_read_err(port,
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

  return result;
}

Scheme_Object *scheme_read_byte_string(Scheme_Object *port)
/* used by GRacket */
{
  return read_string(1, port, NULL, 0);
}

/*========================================================================*/
/*                            vector reader                               */
/*========================================================================*/

/* "#(" has been read */
static Scheme_Object *
read_vector (Scheme_Object *port,
	     int opener, char closer,
	     ReadParams *params,
             int allow_infix)
{
  Scheme_Object *lresult, *obj;
  Scheme_Object *vec;
  int len, i;

  lresult = read_list(port, opener, closer, 
                      (allow_infix ? mz_shape_vec_plus_infix : mz_shape_vec),
                      1, params);

  obj = lresult;

  len = scheme_list_length(obj);
  
  vec = (Scheme_Object *) scheme_make_vector(len, NULL);
  for (i = 0; i < len ; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_CAR(obj);
    obj = SCHEME_CDR(obj);
  }

  return vec;
}

/*========================================================================*/
/*                            symbol reader                               */
/*========================================================================*/

/* Also dispatches to number reader, since things not-a-number are
   symbols. */

typedef int (*Getc_Fun_r)(Scheme_Object *port);

/* nothing has been read, except maybe some flags */
static Scheme_Object  *
read_number_or_symbol(int init_ch, Scheme_Object *port,
		      int is_float, int is_not_float,
		      int radix, int radix_set,
		      int is_symbol, int is_kw,
		      ReadParams *params)
{
  mzchar *buf, *oldbuf, onstack[MAX_QUICK_SYMBOL_SIZE];
  int size, oldsize;
  int i, ch, quoted_ever = 0, running_quote = 0;
  int running_quote_ch = 0;
  Scheme_Object *o;
  int delim_ok;
  int ungetc_ok;
  int far_char_ok;
  int single_escape, multiple_escape;
  Getc_Fun_r getc_fun;

  ungetc_ok = scheme_peekc_is_ungetc(port);

  if (ungetc_ok)
    getc_fun = scheme_getc;
  else
    getc_fun = scheme_peekc;

  i = 0;
  size = MAX_QUICK_SYMBOL_SIZE - 1;
  buf = onstack;

  if (init_ch < 0)
    ch = getc_fun(port);
  else {
    /* Assert: this one won't need to be ungotten */
    ch = init_ch;
  }

  delim_ok = SCHEME_OK;
  far_char_ok = 1;

  while (NOT_EOF_OR_SPECIAL(ch)
	 && (running_quote
	     || (!scheme_isspace(ch) 
		 && (((ch < 128) && (delim[ch] & delim_ok))
		     || ((ch >= 128) && far_char_ok))))) {
    single_escape = (ch == '\\');
    multiple_escape = (ch == '|');
    if (!ungetc_ok) {
      if (init_ch < 0)
	scheme_getc(port); /* must be a character */
      else
	init_ch = -1;
    }
    if (single_escape && !running_quote) {
      int esc_ch = ch;
      ch = scheme_getc(port);
      if (ch == EOF) {
	scheme_read_err(port, "read: EOF following `%c' in %s", esc_ch, is_kw ? "keyword" : "symbol");
	return NULL;
      }
      quoted_ever = 1;
    } else if (multiple_escape && (!running_quote || (ch == running_quote_ch))) {
      quoted_ever = 1;
      running_quote = !running_quote;
      running_quote_ch = ch;

      ch = getc_fun(port);
      continue; /* <-- !!! */
    }

    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
      memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
    }

    buf[i++] = ch;

    ch = getc_fun(port);
  }

  if (ungetc_ok)
    scheme_ungetc(ch, port);

  if (running_quote) {
    scheme_read_err(port, "read: unbalanced `%c`", running_quote_ch);
    return NULL;
  }

  buf[i] = '\0';

  if (!quoted_ever && (i == 1) && (buf[0] == '.')) {
    intptr_t xl, xc, xp;
    scheme_tell_all(port, &xl, &xc, &xp);
    scheme_read_err(port, "read: illegal use of `.'");
    return NULL;
  }

  if ((is_symbol || quoted_ever) && !is_float && !is_not_float && !radix_set)
    o = scheme_false;
  else {
    o = scheme_read_number(buf, i,
			   is_float, is_not_float, 1 /* decimal_inexact */,
			   radix, radix_set,
			   port, NULL, 0);
  }

  if (SAME_OBJ(o, scheme_false)) {
    if (is_kw)
      o = scheme_intern_exact_char_keyword(buf, i);
    else
      o = scheme_intern_exact_char_symbol(buf, i);
  }

  return o;
}

static Scheme_Object  *
read_number(int init_ch,
	    Scheme_Object *port,
	    int is_float, int is_not_float,
	    int radix, int radix_set,
            ReadParams *params)
{
  return read_number_or_symbol(init_ch,
			       port,
			       is_float, is_not_float,
			       radix, radix_set, 0, 0,
			       params);
}

static Scheme_Object  *
read_symbol(int init_ch,
	    Scheme_Object *port,
            ReadParams *params)
{
  return read_number_or_symbol(init_ch,
			       port,
			       0, 0, 10, 0, 1, 0,
			       params);
}

static Scheme_Object  *
read_keyword(int init_ch,
	     Scheme_Object *port,
             ReadParams *params)
{
  return read_number_or_symbol(init_ch,
			       port,
			       0, 0, 10, 0, 1, 1,
			       params);
}

static Scheme_Object  *
read_delimited_constant(int ch, const mzchar *str,
                        Scheme_Object *v,
                        Scheme_Object *port,
                        ReadParams *params)
{
  int first_ch = ch;
  int scanpos = 1;

  if (ch == str[0]) { /* might be `T' instead of `t', for example */
    do {
      ch = scheme_getc(port);
      if ((mzchar)ch == str[scanpos]) {
        scanpos++;
      } else {
        break;
      }
    } while (str[scanpos]);
  } else {
    /* need to show next character to show why it's wrong: */
    ch = scheme_getc(port);
  }

  if (str[scanpos]
      || !next_is_delim(port)) {
    mzchar str_part[7], one_more[2];

    if (!str[scanpos]) {
      /* get non-delimiter again: */
      ch = scheme_getc(port);
    }

    memcpy(str_part, str XFORM_OK_PLUS 1, (scanpos - 1) * sizeof(mzchar));
    str_part[scanpos - 1] = 0;
    if (NOT_EOF_OR_SPECIAL(ch)) {
      one_more[0] = ch;
      one_more[1] = 0;
    } else
      one_more[0] = 0;

    scheme_read_err(port,
                    "read: bad syntax `#%c%5%u'",
                    first_ch,
                    str_part,
                    one_more, 
                    (intptr_t)(NOT_EOF_OR_SPECIAL(ch) ? 1 : 0));
    return NULL;
  }
            
  return v;
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

static Scheme_Object *make_interned_char(int ch, int intern)
{
  if (ch < 256)
    return scheme_make_character(ch);
  else if (intern)
    return scheme_intern_literal_number(scheme_make_char(ch));
  else
    return scheme_make_char(ch);
}

/* "#\" has been read */
static Scheme_Object *
read_character(Scheme_Object *port,
               ReadParams *params)
{
  int ch, next;

  ch = scheme_getc(port);

  next = scheme_peekc(port);

  if ((ch >= '0' && ch <= '7') && (next >= '0' && next <= '7')) {
    /* a is the same as next */
    int last;

    last = (scheme_getc(port) /* is char */, scheme_peekc(port));

    if (last != SCHEME_SPECIAL)
      scheme_getc(port); /* must be last */

    if (last < '0' || last > '7' || ch > '3') {
      scheme_read_err(port,
		      "read: bad character constant #\\%c%c%c",
		      ch, next, ((last == EOF) || (last == SCHEME_SPECIAL)) ? ' ' : last);
      return NULL;
    }

    ch = ((ch - '0') << 6) + ((next - '0') << 3) + (last - '0');

    return make_interned_char(ch, 0);
  }

  if (((ch == 'u') || (ch == 'U')) && NOT_EOF_OR_SPECIAL(next) && scheme_isxdigit(next)) {
    int count = 0, n = 0, nbuf[10], maxc = ((ch == 'u') ? 4 : 8);
    while (count < maxc) {
      ch = scheme_peekc(port);
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
      scheme_read_err(port,
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
    while ((ch = scheme_peekc(port), NOT_EOF_OR_SPECIAL(ch) && scheme_isalpha(ch))) {
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

    scheme_read_err(port, "read: bad character constant: #\\%5", buf);
  }

  if (ch == EOF)
    scheme_read_err(port, "read: expected a character after #\\");

  return make_interned_char(ch, 0);
}

/*========================================================================*/
/*                            quote readers                               */
/*========================================================================*/

/* "'", etc. has been read */
static Scheme_Object *
read_quote(char *who, Scheme_Object *quote_symbol, int len,
	   Scheme_Object *port, ReadParams *params)
{
  Scheme_Object *obj, *ret;

  obj = read_inner(port, params, -1);
  if (SCHEME_EOFP(obj))
    scheme_read_err(port, "read: expected an element for %s (found end-of-file)", who);
  ret = quote_symbol;
  ret = scheme_make_pair(ret, scheme_make_pair(obj, scheme_null));
  return ret;
}

/* "#&" has been read */
static Scheme_Object *read_box(Scheme_Object *port, ReadParams *params)
{
  Scheme_Object *o, *bx;

  o = read_inner(port, params, -1);

  if (SCHEME_EOFP(o))
    scheme_read_err(port, "read: expected an element for #& box (found end-of-file)");

  bx = scheme_box(o);

  return bx;
}

/*========================================================================*/
/*                         hash table reader                              */
/*========================================================================*/

/* "(" has been read */
static Scheme_Object *read_hash(Scheme_Object *port,
				int opener, char closer,  int kind,
                                ReadParams *params)
{
  Scheme_Object *l;
  Scheme_Object *key, *val;
  Scheme_Hash_Tree *t;

  /* using mz_shape_hash_list ensures that l is a list of pairs */
  l = read_list(port, opener, closer, mz_shape_hash_list, 0, params);

  t = scheme_make_hash_tree(kind);

  for (; SCHEME_STX_PAIRP(l); l = SCHEME_STX_CDR(l)) {
    val = SCHEME_STX_CAR(l);
    key = SCHEME_STX_CAR(val);
    key = scheme_syntax_to_datum(key);
    key = scheme_expander_syntax_to_datum(key);
    val = SCHEME_STX_CDR(val);
    
    t = scheme_hash_tree_set(t, key, val);
  }
    
  return (Scheme_Object *)t;
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
skip_whitespace_comments(Scheme_Object *port,
                         ReadParams *params)
{
  int ch;

 start_over:

  while ((ch = scheme_getc(port), NOT_EOF_OR_SPECIAL(ch) && scheme_isspace(ch))) {}

  if (ch == ';') {
    do {
      ch = scheme_getc(port);
    } while (!is_line_comment_end(ch) && (ch != EOF));
    goto start_over;
  }

  if ((ch == '#')
      && (scheme_peekc(port) == '|')) {
    int depth = 0;
    int ch2 = 0;

    (void)scheme_getc(port); /* re-read '|' */
    do {
      ch = scheme_getc(port);
 
      if (ch == EOF)
	scheme_read_err(port, "read: end of file in #| comment");

      if ((ch2 == '|') && (ch == '#')) {
	if (!(depth--))
	  goto start_over;
	ch = 0; /* So we don't count '#' toward an opening "#|" */
      } else if ((ch2 == '#') && (ch == '|')) {
	depth++;
	ch = 0; /* So we don't count '|' toward a closing "|#" */
      }
      ch2 = ch;
    } while (1);

    goto start_over;
  }
  if ((ch == '#')
      && (scheme_peekc(port) == ';')) {
    Scheme_Object *skipped;

    (void)scheme_getc(port); /* re-read ';' */

    skipped = read_inner(port, params, -1);
    if (SCHEME_EOFP(skipped))
      scheme_read_err(port, "read: expected a commented-out element for `#;' (found end-of-file)");

    goto start_over;
  }

  return ch;
}

static void unexpected_closer(int ch, Scheme_Object *port)
{
  scheme_read_err(port, "read: unexpected `%c`", ch);
}

static int read_graph_index(Scheme_Object *port, int *ch)
{
  int digits = 0, val = 0, nch;

  while (NOT_EOF_OR_SPECIAL((*ch)) && isdigit_ascii((*ch))) {
    if (digits >= MAX_GRAPH_ID_DIGITS)
      scheme_read_err(port, "too many digits after `#%d`", val);
    digits++;

    val = ((val) * 10) + ((*ch) - 48);
    nch = scheme_getc(port);
    (*ch) = nch;
  }

  return val;
}

/*========================================================================*/
/*                               .zo reader                               */
/*========================================================================*/

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
  Scheme_Object *relto;
  intptr_t *shared_offsets;
  Scheme_Load_Delay *delay_info;
  mzlonglong bytecode_hash;
} CPort;
#define CP_GETC(cp) ((int)(cp->start[cp->pos++]))
#define CP_UNGETC(cp) --cp->pos
#define CP_TELL(port) (port->pos + port->base)

typedef void *(*GC_Alloc_Proc)(size_t);

static Scheme_Object *read_compact_list(int c, int proper, int use_stack, CPort *port);
static Scheme_Object *read_compact_quote(CPort *port, int embedded);

void scheme_ill_formed(struct CPort *port
#if TRACK_ILL_FORMED_CATCH_LINES
		       , const char *file, int line
#endif
		       )
{
  scheme_read_err(port ? port->orig_port : NULL, 
		  "read (compiled): ill-formed code"
#if TRACK_ILL_FORMED_CATCH_LINES
		  " [%s:%d]", file, line
#endif
		  );
}

static void make_ut(CPort *port)
{
  Scheme_Unmarshal_Tables *ut;
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
                                              Scheme_Hash_Table **ht,
                                              Scheme_Object *orig_port)
{
  Scheme_Object *ep, *v;
  ReadParams params;
  Scheme_Input_Port *ep_ip;

  ep = scheme_make_sized_byte_string_input_port(s, len);

  if (orig_port) {
    v = scheme_input_port_record(orig_port)->name;
    if (v) {
      ep_ip = scheme_input_port_record(ep);
      ep_ip->name = v;
    }
  }
    
  params.skip_zo_vers_check = 0;
  params.read_relative_path = rel_to;
  params.graph_ht = *ht;

  v = read_inner(ep, &params, -1);

  *ht = params.graph_ht;

  return v;
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

  return read_escape_from_string(s, len, port->relto, port->ht, port->orig_port);
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
    
    v = resolve_references(v, port->orig_port,
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
      return make_interned_char(l, 1);
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
        Scheme_Hash_Tree *ht;
	int kind, len;
        Scheme_Object *k;

	kind = read_compact_number(port);
        if ((kind < 0) || (kind > 2))
          scheme_ill_formed_code(port);
	len = read_compact_number(port);

        ht = scheme_make_hash_tree(kind);
        while (len--) {
	  k = read_compact(port, 0);
	  v = read_compact(port, 0);
          ht = scheme_hash_tree_set(ht, k, v);
	}

	v = (Scheme_Object *)ht;
      }
      break;
    case CPT_LINKLET:
      {
        int has_prefix;
        Scheme_Prefix *pf;

        has_prefix = read_compact_number(port);
        if (has_prefix)
          pf = (Scheme_Prefix *)read_compact(port, 0);
        else
          pf = NULL;

        v = read_compact(port, 1);
        v = scheme_read_linklet(v, port->unsafe_ok);
        if (!v) scheme_ill_formed_code(port);

        ((Scheme_Linklet *)v)->static_prefix = pf;

        return v;
      }
      break;
    case CPT_QUOTE:
      v = read_compact_quote(port, 1);
      break;
    case CPT_REFERENCE:
      l = read_compact_number(port);
      RANGE_CHECK(l, < EXPECTED_PRIM_COUNT);
      return variable_references[l];
      break;
    case CPT_TOPLEVEL:
      {
        int flags, pos, depth;

        flags = read_compact_number(port);
        pos = read_compact_number(port);
        depth = read_compact_number(port);

        if ((depth < 0) || (pos < 0))
          scheme_ill_formed_code(port);

        return scheme_make_toplevel(depth, pos, flags & SCHEME_TOPLEVEL_FLAGS_MASK);
      }
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
    case CPT_BEGIN:
    case CPT_BEGIN0:
      {
        Scheme_Sequence *seq;
        int i, count;

        count = read_compact_number(port);
        if (count <= 0) scheme_ill_formed_code(port);
        seq = scheme_malloc_sequence(count);
        seq->so.type = ((ch == CPT_BEGIN) ? scheme_sequence_type : scheme_begin0_sequence_type);
        seq->count = count;

        for (i = 0; i < count; i++) {
          v = read_compact(port, 1);
          seq->array[i] = v;
        }

        return (Scheme_Object *)seq;
      }
      break;
    case CPT_LET_VALUE:
      {
        Scheme_Let_Value *lv;
        int c, p;
 
        lv = (Scheme_Let_Value *)scheme_malloc_tagged(sizeof(Scheme_Let_Value));
        lv->iso.so.type = scheme_let_value_type;

        c = read_compact_number(port);
        p = read_compact_number(port);
        if ((c < 0) || (p < 0)) scheme_ill_formed_code(port);
        
        lv->count = c;
        lv->position = p;
        if (read_compact_number(port))
          SCHEME_LET_VALUE_AUTOBOX(lv) = 1;
        v = read_compact(port, 1);
        lv->value = v;
        v = read_compact(port, 1);
        lv->body = v;
        
        return (Scheme_Object *)lv;
      }
      break;
    case CPT_LET_VOID:
      {
        Scheme_Let_Void *lv;
        int c;
 
        lv = (Scheme_Let_Void *)scheme_malloc_tagged(sizeof(Scheme_Let_Void));
        lv->iso.so.type = scheme_let_void_type;

        c = read_compact_number(port);
        if (c < 0) scheme_ill_formed_code(port);

        lv->count = c;
        if (read_compact_number(port))
          SCHEME_LET_VOID_AUTOBOX(lv) = 1;
        v = read_compact(port, 1);
        lv->body = v;
        
        return (Scheme_Object *)lv;
      }
      break;
    case CPT_LETREC:
      {
        Scheme_Letrec *lr;
        Scheme_Object **sa;
        int i, c;

        lr = MALLOC_ONE_TAGGED(Scheme_Letrec);
        lr->so.type = scheme_letrec_type;

        c = read_compact_number(port);
        if (c < 0) scheme_ill_formed_code(port);
        
        lr->count = c;
        if (c < 4096)
          sa = MALLOC_N(Scheme_Object*, c);
        else {
          sa = scheme_malloc_fail_ok(scheme_malloc, scheme_check_overflow(c, sizeof(Scheme_Object *), 0));
          if (!sa) scheme_signal_error("out of memory allocating letrec bytecode");
        }
        lr->procs = sa;

        for (i = 0; i < c; i++) {
          v = read_compact(port, 1);
          sa[i] = v;
        }

        v = read_compact(port, 1);
        lr->body = v;

        return (Scheme_Object *)lr;
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
    case CPT_WCM:
      {
        Scheme_With_Continuation_Mark *wcm;
        
        wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
        wcm->so.type = scheme_with_cont_mark_type;

        v = read_compact(port, 1);
        wcm->key = v;
        v = read_compact(port, 1);
        wcm->val = v;
        v = read_compact(port, 1);
        wcm->body = v;

	return (Scheme_Object *)wcm;
      }
      break;
    case CPT_DEFINE_VALUES:
      {
        v = read_compact(port, 1);
        if (!SCHEME_VECTORP(v)) scheme_ill_formed_code(port);
        {
          int i, c = SCHEME_VEC_SIZE(v);
          if (c < 1) scheme_ill_formed_code(port);
          for (i = 1; i < c; i++) {
            if (!SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(v)[i]), scheme_toplevel_type)
                && !SAME_TYPE(SCHEME_TYPE(SCHEME_VEC_ELS(v)[i]), scheme_static_toplevel_type))
              scheme_ill_formed_code(port);
          }
        }
        v->type = scheme_define_values_type;
        return v;
      }
      break;
    case CPT_SET_BANG:
      {
        Scheme_Set_Bang *sb;

        sb = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
        sb->so.type = scheme_set_bang_type;

        if (read_compact_number(port))
          sb->set_undef = 1;

        v = read_compact(port, 1);
        sb->var = v;
        if (!SAME_TYPE(SCHEME_TYPE(v), scheme_toplevel_type)
            && !SAME_TYPE(SCHEME_TYPE(v), scheme_static_toplevel_type))
          scheme_ill_formed_code(port);
        v = read_compact(port, 1);
        sb->val = v;

        return (Scheme_Object *)sb;
      }
      break;
    case CPT_OTHER_FORM:
      {
        switch (read_compact_number(port)) {
        case scheme_static_toplevel_type:
          {
            Scheme_Object *tl = scheme_false;
            Scheme_Prefix *pf;
            intptr_t flags, pos, i;

            flags = read_compact_number(port);
            pos = read_compact_number(port);

            /* Avoid recur on very common case of a reference to the prefix: */
            ch = CP_GETC(port);
            if (ch == CPT_SYMREF) {
              l = read_compact_number(port);
              RANGE_POS_CHECK(l, < port->symtab_size);
              pf = (Scheme_Prefix *)port->symtab[l];
            } else {
              CP_UNGETC(port);
              pf = (Scheme_Prefix *)read_compact(port, 0);
            }

            if (!SAME_TYPE(SCHEME_TYPE(pf), scheme_prefix_type) || (pos < 0) || (pos >= pf->num_slots))
              scheme_ill_formed_code(port);

            flags &= SCHEME_TOPLEVEL_FLAGS_MASK;
            i = ((pos << SCHEME_LOG_TOPLEVEL_FLAG_MASK) | flags);
            if ((i < 0) || (i >= (pf->num_slots * (SCHEME_TOPLEVEL_FLAGS_MASK + 1))))
              scheme_ill_formed_code(port);

            tl = ((Scheme_Object **)pf->a[pf->num_slots-1])[i];
            if (!tl) {
              tl = (Scheme_Object *)MALLOC_ONE_TAGGED(Scheme_Toplevel);
              tl->type = scheme_static_toplevel_type;
              SCHEME_STATIC_TOPLEVEL_PREFIX(tl) = pf;
              SCHEME_TOPLEVEL_POS(tl) = pos;
              SCHEME_TOPLEVEL_FLAGS(tl) |= flags;
              ((Scheme_Object **)pf->a[pf->num_slots-1])[i] = tl;
            }
        
            return tl;
          }
          break;
        case scheme_prefix_type:
          {
            intptr_t prefix_size;
            Scheme_Object **a;

            prefix_size = read_compact_number(port);
            if (prefix_size <= 0) scheme_ill_formed_code(port);
            if (prefix_size < 4096)
              v = (Scheme_Object *)scheme_allocate_prefix(prefix_size);
            else
              v = scheme_malloc_fail_ok((GC_Alloc_Proc)scheme_allocate_prefix, prefix_size);

            /* Last prefix slot is a cache of Scheme_Toplevel values */
            a = MALLOC_N(Scheme_Object *, prefix_size * (SCHEME_TOPLEVEL_FLAGS_MASK + 1));
            ((Scheme_Prefix *)v)->a[prefix_size-1] = (Scheme_Object *)a;

            return v;
          }
        case scheme_boxenv_type:
          {
            Scheme_Object *data;
            
            data = scheme_alloc_object();
            data->type = scheme_boxenv_type;
            
            v = read_compact(port, 1);
            SCHEME_PTR1_VAL(data) = v;
            v = read_compact(port, 1);
            SCHEME_PTR2_VAL(data) = v;
            
            return data;
          }
          break;
        case scheme_with_immed_mark_type:
          {
            Scheme_With_Continuation_Mark *wcm;
            
            wcm = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
            wcm->so.type = scheme_with_immed_mark_type;
            
            v = read_compact(port, 1);
            wcm->key = v;
            v = read_compact(port, 1);
            wcm->val = v;
            v = read_compact(port, 1);
            wcm->body = v;
            
            return (Scheme_Object *)wcm;
          }
        case scheme_inline_variant_type:
          {
            Scheme_Object *data;
            
            data = scheme_make_vector(3, scheme_false);
            data->type = scheme_inline_variant_type;

            v = read_compact(port, 1);
            SCHEME_VEC_ELS(data)[0] = v;
            v = read_compact(port, 1);
            SCHEME_VEC_ELS(data)[1] = v;
            /* third slot is filled when linklet->accessible table is made */

            return data;
          }
        case scheme_case_lambda_sequence_type:
          {
            int count, i, all_closed = 1;
            Scheme_Case_Lambda *cl;

            count = read_compact_number(port);
            if (count < 0) scheme_ill_formed_code(port);

            if (count < 4096)
              cl = (Scheme_Case_Lambda *)scheme_malloc_tagged(sizeof(Scheme_Case_Lambda)
                                                              + (count - mzFLEX_DELTA) * sizeof(Scheme_Object *));
            else {
              intptr_t sz;
              sz = scheme_check_overflow((count - mzFLEX_DELTA), sizeof(Scheme_Object *), sizeof(Scheme_Case_Lambda));
              cl = (Scheme_Case_Lambda *)scheme_malloc_fail_ok(scheme_malloc_tagged, sz);
              if (!cl) scheme_signal_error("out of memory allocating procedure bytecode");
            }

            cl->so.type = scheme_case_lambda_sequence_type;
            cl->count = count;

            v = read_compact(port, 1);
            if (SCHEME_NULLP(v))
              cl->name = NULL;
            else
              cl->name = v;

            for (i = 0; i < count; i++) {
              v = read_compact(port, 1);
              cl->array[i] = v;
              if (!SCHEME_PROCP(v)) {
                if (!SAME_TYPE(SCHEME_TYPE(v), scheme_lambda_type))
                  scheme_ill_formed_code(port);
                all_closed = 0;
              } else if (!SAME_TYPE(SCHEME_TYPE(v), scheme_closure_type))
                scheme_ill_formed_code(port);
            }

            if (all_closed) {
              /* Empty closure: produce procedure value directly.
                 (We assume that this was generated by a direct write of
                 a case-lambda data record in print.c, and that it's not
                 in a CASE_LAMBDA_EXPD syntax record.) */
              return scheme_case_lambda_execute((Scheme_Object *)cl);
            }
            
            return (Scheme_Object *)cl;
          }
          break;
        case scheme_lambda_type:
          {
            Scheme_Object *name, *ds, *closure_map, *tl_map;
            int flags, closure_size, num_params, max_let_depth;

            flags = read_compact_number(port);
            if (flags & LAMBDA_HAS_TYPED_ARGS)
              closure_size = read_compact_number(port);
            else
              closure_size = -1;
            num_params = read_compact_number(port);
            max_let_depth = read_compact_number(port);

            name = read_compact(port, 1);
            ds = read_compact(port, 1);
            closure_map = read_compact(port, 1);
            tl_map = read_compact(port, 1);

            v = scheme_read_lambda(flags, closure_size, num_params, max_let_depth,
                                   name, ds, closure_map, tl_map);
            if (!v) scheme_ill_formed_code(port);

            return v;
          }
        default:
          scheme_ill_formed_code(port);
          return NULL;
          break;
        }
      }
      break;
    case CPT_VARREF:
      {
        Scheme_Object *data;
        int flags;

        data = scheme_alloc_object();
        data->type = scheme_varref_form_type;

        flags = read_compact_number(port);
        SCHEME_VARREF_FLAGS(data) |= (flags & VARREF_FLAGS_MASK);

        v = read_compact(port, 1);
        SCHEME_PTR1_VAL(data) = v;
        if (!SCHEME_SYMBOLP(v)
            && !SCHEME_FALSEP(v)
            && !SAME_OBJ(v, scheme_true)
            && !SAME_TYPE(SCHEME_TYPE(v), scheme_toplevel_type))
          scheme_ill_formed_code(port);

        v = read_compact(port, 1);
        SCHEME_PTR2_VAL(data) = v;
        if (!SCHEME_FALSEP(v)
            && !SAME_TYPE(SCHEME_TYPE(v), scheme_toplevel_type))
          scheme_ill_formed_code(port);

        return data;
      }
      break;
    case CPT_APPLY_VALUES:
      {
        Scheme_Object *data;

        data = scheme_alloc_object();
        data->type = scheme_apply_values_type;

        v = read_compact(port, 1);
        SCHEME_PTR1_VAL(data) = v;
        v = read_compact(port, 1);
        SCHEME_PTR2_VAL(data) = v;

        return data;
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
    case CPT_SRCLOC:
      {
        Scheme_Object *r;
        r = scheme_unsafe_make_location();
        /* No checking of field values, so a corrupt ".zo" can
           create bad srclocs (but won't crash while reading) */
        v = read_compact(port, 0);
        ((Scheme_Structure *)r)->slots[0] = v;
        v = read_compact(port, 0);
        ((Scheme_Structure *)r)->slots[1] = v;
        v = read_compact(port, 0);
        ((Scheme_Structure *)r)->slots[2] = v;
        v = read_compact(port, 0);
        ((Scheme_Structure *)r)->slots[3] = v;
        v = read_compact(port, 0);
        ((Scheme_Structure *)r)->slots[4] = v;
        return r;
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
               loading the lamda form on demand. */
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
    case CPT_SMALL_SYMBOL_START:
      {
	l = ch - CPT_SMALL_SYMBOL_START;
	RANGE_CHECK_GETS(l);
	s = read_compact_chars(port, buffer, BLK_BUF_SIZE, l);
        if (!valid_utf8(s, l))
          scheme_ill_formed_code(port);
	v = scheme_intern_exact_symbol(s, l);
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
    v = resolve_references(v, port->orig_port,
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           scheme_make_hash_table(SCHEME_hash_ptr),
                           port->symtab_entries,
                           0, 0);

  return v;
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

static Scheme_Object *read_linklet_bundle_hash(Scheme_Object *port,
                                               int can_read_unsafe,
                                               Scheme_Object *delay_load_info)
{
  Scheme_Object *result;
  intptr_t size, shared_size, got, offset;
  CPort *rp;
  intptr_t symtabsize;
  Scheme_Object **symtab;
  intptr_t *so;
  Scheme_Load_Delay *delay_info;
  Scheme_Hash_Table **local_ht;
  int all_short;
  int perma_cache = use_perma_cache;
  Scheme_Object *dir;
  Scheme_Config *config;
  Scheme_Performance_State perf_state;

  scheme_performance_record_start(&perf_state);

  /* Allow delays? */
  if (delay_load_info) {
    delay_info = MALLOC_ONE_RT(Scheme_Load_Delay);
    SET_REQUIRED_TAG(delay_info->type = scheme_rt_delay_load_info);
    delay_info->path = delay_load_info;
  } else
    delay_info = NULL;

  symtabsize = read_simple_number_from_port(port);
  
  /* Load table mapping symtab indices to stream positions: */

  all_short = scheme_get_byte(port);
  if (symtabsize < 0)
    so = NULL;
  else
    so = (intptr_t *)scheme_malloc_fail_ok(scheme_malloc_atomic, 
                                           scheme_check_overflow(symtabsize, sizeof(intptr_t), 0));
  if (!so)
    scheme_read_err(port,
                    "read (compiled): could not allocate symbol table of size %" PRIdPTR,
                    symtabsize);
  if ((got = scheme_get_bytes(port, (all_short ? 2 : 4) * (symtabsize - 1), (char *)so, 0)) 
      != ((all_short ? 2 : 4) * (symtabsize - 1)))
    scheme_read_err(port,
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
    scheme_read_err(port,
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
    scheme_read_err(port,
                    "read (compiled): ill-formed code (bad count: %ld != %ld"
                    ", started at %ld)",
                    got, size, rp->base);

  local_ht = MALLOC_N(Scheme_Hash_Table *, 1);

  symtab = MALLOC_N(Scheme_Object *, symtabsize);
  rp->symtab_size = symtabsize;
  rp->ht = local_ht;
  rp->symtab = symtab;
  rp->unsafe_ok = can_read_unsafe;

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
  result = read_compact(rp, 1);

  if (delay_info) {
    if (delay_info->ut)
      delay_info->ut->rp = NULL; /* clean up */
  }

  if (*local_ht)
    scheme_read_err(port, "read (compiled): unexpected graph structure");

  if (!SCHEME_HASHTRP(result))
    scheme_read_err(port, "read (compiled): bundle content is not an immutable hash");

  {
    mzlonglong i;
    Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)result;
    Scheme_Object *key, *val;

    if (!scheme_starting_up) {
      i = scheme_hash_tree_next(t, -1);
      while (i != -1) {
        scheme_hash_tree_index(t, i, &key, &val);
        if (validate_loaded_linklet
            && SAME_TYPE(SCHEME_TYPE(val), scheme_linklet_type)
            && !((Scheme_Linklet *)val)->reject_eval)
          scheme_validate_linklet(rp, (Scheme_Linklet *)val);
        i = scheme_hash_tree_next(t, i);
      }
    }
      
    /* If no exception, the resulting code is ok. */
  }

  scheme_performance_record_end("read", &perf_state);
  return result;
}

Scheme_Object *scheme_read_linklet_bundle_hash(Scheme_Object *port)
{
  Scheme_Config *config;
  int can_read_unsafe;
  Scheme_Object *delay_load_info, *v, *v2;

  config = scheme_current_config();

  v = scheme_get_param(scheme_current_config(), MZCONFIG_CODE_INSPECTOR);
  v2 = scheme_get_initial_inspector();
  can_read_unsafe = SAME_OBJ(v, v2);

  v = scheme_get_param(config, MZCONFIG_DELAY_LOAD_INFO);
  if (SCHEME_TRUEP(v))
    delay_load_info = v;
  else
    delay_load_info = NULL;

  return read_linklet_bundle_hash(port, can_read_unsafe, delay_load_info);
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
  Scheme_Performance_State perf_state;

  scheme_performance_record_start(&perf_state);
  
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
    port = scheme_do_open_input_file("on-demand-loader", 0, 1, a, 1, 0);

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
        scheme_read_err(port,
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
    if (*ht) {
      scheme_read_err(rp->orig_port, "read (compiled): unexpected graph structure");
    }
  }
  scheme_current_thread->error_buf = savebuf;
  scheme_current_thread->reading_delayed = NULL;

  /* Clean up: */
  v = resolve_symtab_refs(v, rp);

  delay_info->current_rp = old_rp;
  if (delay_info->ut)
    delay_info->ut->rp = old_rp;

  if (!old_rp && !delay_info->perma_cache) {
    /* No one using the cache, to register it to be cleaned up */
    delay_info->clear_bytes_next = clear_bytes_chain;
    if (clear_bytes_chain)
      clear_bytes_chain->clear_bytes_prev = delay_info;
    clear_bytes_chain = delay_info;
  }

  scheme_end_atomic_no_swap();

  scheme_performance_record_end("demand-read", &perf_state);

  if (v) {
    /* Although `which` is a symbol-table index for `v`,
       we don't actually record v, because the delayed
       reference is now complete (and we'd like to be
       able to GC it if it's otherwise unused). */
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
/*                         precise GC traversers                          g*/
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_read.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_indent_type, mark_indent);
  GC_REG_TRAV(scheme_rt_compact_port, mark_cport);
  GC_REG_TRAV(scheme_rt_read_params, mark_read_params);
  GC_REG_TRAV(scheme_rt_delay_load_info, mark_delay_load);
  GC_REG_TRAV(scheme_rt_unmarshal_info, mark_unmarshal_tables);
}

END_XFORM_SKIP;

#endif
