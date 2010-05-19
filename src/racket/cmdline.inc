/****************************************************************/
/* This command-line parser is used by both Racket and Gracket. */
/****************************************************************/

#pragma GCC diagnostic ignored "-Wwrite-strings"

#define SDESC "Set! works on undefined identifiers"

char *cmdline_exe_hack = 
  ("[Replace me for EXE hack                                       "
   "                                                              ]");

#ifdef MZ_PRECISE_GC
# define GC_PRECISION_TYPE "3"
#else
# define GC_PRECISION_TYPE "c"
#endif
char *binary_type_hack = "bINARy tYPe:" INITIAL_BIN_TYPE GC_PRECISION_TYPE;
/* The format of bINARy tYPe is e?[zr]i[3c].
   e indicates a starter executable
   z/r indicates Racket or GRacket
   i indicates ???
   3/c indicates 3m or CGC */

#ifndef INITIAL_COLLECTS_DIRECTORY
# ifdef DOS_FILE_SYSTEM
#  define INITIAL_COLLECTS_DIRECTORY "collects"
# else
#  define INITIAL_COLLECTS_DIRECTORY "../collects"
# endif
#endif

static char *_coldir = "coLLECTs dIRECTORy:" /* <- this tag stays, so we can find it again */
                       INITIAL_COLLECTS_DIRECTORY 
                       "\0\0" /* <- 1st nul terminates path, 2nd terminates path list */
                       /* Pad with at least 1024 bytes: */
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************"
                       "****************************************************************";
static int _coldir_offset = 19; /* Skip permanent tag */

#ifndef MZ_PRECISE_GC
# define XFORM_OK_PLUS +
#endif

#ifdef DOS_FILE_SYSTEM
# include <Windows.h>

#define DLL_RELATIVE_PATH L"lib"
#include "delayed.inc"

extern 
# ifdef __cplusplus
"C"
# endif
__declspec(dllexport) void scheme_set_dll_path(wchar_t *s);

static void record_dll_path(void)
{
  if (_dlldir[_dlldir_offset] != '<') {
    scheme_set_dll_path(_dlldir + _dlldir_offset);
  }
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif
#endif

#ifndef DONT_PARSE_COMMAND_LINE
static int is_number_arg(const char *s)
{
  while (*s) {
    if (*s < '0' || *s > '9') {
      if (*s == '.') {
	s++;
	while (*s) {
	  if (*s < '0' || *s > '9')
	    return 0;
	  else
	    s++;
	}
	return 1;
      } else
	return 0;
    } else
      s++;
  }

  return 1;
}

static char *make_embedded_load(const char *start, const char *end)
{
  char *s;
  int slen, elen;

  slen = strlen(start);
  elen = strlen(end);

  s = (char *)malloc(slen + elen + 2);
  memcpy(s, start, slen + 1);
  memcpy(s + slen + 1, end, elen + 1);

  return s;
}
#endif

enum {
  mzcmd_EVAL            = 0,
  mzcmd_LOAD            = 1,
  mzcmd_MAIN            = 2,
  mzcmd_REQUIRE_FILE    = 3,
  mzcmd_REQUIRE_LIB     = 4,
  mzcmd_REQUIRE_PLANET  = 5,
  mzcmd_EMBEDDED        = 6,
};

/* To avoid having to create a specific mark procedure for
   prcise GC, split argument information into purely atomic
   and purely non-atomic records. */

typedef struct {
  int init_ns;
#ifndef DONT_PARSE_COMMAND_LINE
  int num_enl;
#endif  
#ifndef DONT_LOAD_INIT_FILE
  int no_init_file;
#endif
#ifndef DONT_RUN_REP
  int use_repl;
  int script_mode;
#endif
#ifdef YIELD_BEFORE_EXIT
  int add_yield;
#endif
#ifdef CMDLINE_STDIO_FLAG
  int alternate_rep;
  int no_front;
#endif
} FinishArgsAtoms;

typedef struct {
  FinishArgsAtoms *a;
#ifndef DONT_PARSE_COMMAND_LINE
  char **evals_and_loads;
  int *eval_kind;
  Scheme_Object *main_args;
#endif  
  Scheme_Env *global_env;
  char *init_lib;
} FinishArgs;

typedef void (*Repl_Proc)(Scheme_Env *);

static void configure_environment(Scheme_Object *mod)
{
  Scheme_Object *mli, *dyreq, *a[3], *gi, *v, *vs;

  mli = scheme_builtin_value("module->language-info");

  a[0] = mod;
  a[1] = scheme_make_true();
  v = scheme_apply(mli, 2, a);
  if (SCHEME_VECTORP(v) && SCHEME_VEC_SIZE(v) == 3) {
    dyreq = scheme_builtin_value("dynamic-require");

    a[0] = SCHEME_VEC_ELS(v)[0];
    a[1] = SCHEME_VEC_ELS(v)[1];
    gi = scheme_apply(dyreq, 2, a);

    a[0] = SCHEME_VEC_ELS(v)[2];
    gi = scheme_apply(gi, 1, a);

    a[0] = scheme_intern_symbol("configure-runtime");
    a[1] = scheme_make_null();
    vs = scheme_apply(gi, 2, a);
    a[0] = vs;
    while (SCHEME_PAIRP(vs)) {
      v = SCHEME_CAR(vs);
      vs = SCHEME_CDR(vs);
      if (SCHEME_VECTORP(v) && SCHEME_VEC_SIZE(v) == 3) {
        /* ok */
      } else {
        break;
      }
    }
    if (!SAME_OBJ(vs, scheme_make_null())) {
      scheme_wrong_type("runtime-configure", "list of vectors of three values",
                        -1, 0, a);
    }

    vs = a[0];
    while (SCHEME_PAIRP(vs)) {
      v = SCHEME_CAR(vs);
      vs = SCHEME_CDR(vs);

      a[0] = SCHEME_VEC_ELS(v)[0];
      a[1] = SCHEME_VEC_ELS(v)[1];
      a[2] = SCHEME_VEC_ELS(v)[2];
      v = scheme_apply(dyreq, 2, a);
      
      a[0] = a[2];
      scheme_apply_multi(v, 1, a);
    }
  }
}

static int finish_cmd_line_run(FinishArgs *fa, Repl_Proc repl)
{
  volatile int exit_val = 0;
  volatile int did_config = 0;

  if (fa->a->init_ns) {
    Scheme_Object *a[1], *nsreq;
    Scheme_Thread * volatile p;
    mz_jmp_buf * volatile save, newbuf;

    nsreq = scheme_builtin_value("namespace-require");

    a[0] = scheme_make_pair(scheme_intern_symbol("lib"),
                            scheme_make_pair(scheme_make_utf8_string(fa->init_lib),
                                             scheme_make_null()));

    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      if (!did_config) {
        configure_environment(a[0]);
        did_config = 1;
      }
      scheme_apply(nsreq, 1, a);
    } else {
      exit_val = 1;
    }        
    p->error_buf = save;
  }

#ifndef DONT_PARSE_COMMAND_LINE
  {
    volatile int i;
    mz_jmp_buf * volatile save, newbuf;

    for (i = 0; i < fa->a->num_enl; i++) {
      if ((fa->eval_kind[i] == mzcmd_LOAD)
          && strcmp(fa->evals_and_loads[i], "-")) {
        if (!scheme_load(fa->evals_and_loads[i])) {
	  exit_val = 1;
	  break;
	}
      } else if ((fa->eval_kind[i] == mzcmd_EVAL)
                 || (fa->eval_kind[i] == mzcmd_LOAD) /* stdin */
                 || (fa->eval_kind[i] == mzcmd_REQUIRE_FILE)
                 || (fa->eval_kind[i] == mzcmd_REQUIRE_LIB)
                 || (fa->eval_kind[i] == mzcmd_REQUIRE_PLANET)
                 || (fa->eval_kind[i] == mzcmd_EMBEDDED)) {
	Scheme_Thread * volatile p;
	p = scheme_get_current_thread();
	save = p->error_buf;
	p->error_buf = &newbuf;
        
        if (!scheme_setjmp(newbuf)) {
          if (fa->eval_kind[i] == mzcmd_EVAL) {
            scheme_eval_string_all_with_prompt(fa->evals_and_loads[i], fa->global_env, 2);
          } else if (fa->eval_kind[i] == mzcmd_LOAD) {
            /* eval from stdin */
            scheme_eval_all_with_prompt(NULL, fa->global_env, 2);
          } else if (fa->eval_kind[i] == mzcmd_EMBEDDED) {
            Scheme_Object *s, *e, *a[3], *eload;
            eload = scheme_builtin_value("embedded-load");
            s = scheme_make_utf8_string(fa->evals_and_loads[i]);
            e = scheme_make_utf8_string(fa->evals_and_loads[i] + strlen(fa->evals_and_loads[i]) + 1);
            a[0] = s;
            a[1] = e;
            a[2] = scheme_make_false();
            scheme_apply(eload, 3, a);
          } else {
            Scheme_Object *a[1], *nsreq;
            char *name;
            nsreq = scheme_builtin_value("namespace-require");
            if (fa->eval_kind[i] == mzcmd_REQUIRE_LIB) {
              name = "lib";
            } else if (fa->eval_kind[i] == mzcmd_REQUIRE_PLANET) {
              name = "planet";
            } else {
              name = "file";
            }
            a[0] = scheme_make_pair(scheme_intern_symbol(name),
                                    scheme_make_pair(scheme_make_utf8_string(fa->evals_and_loads[i]),
                                                     scheme_make_null()));
            if (!did_config)
              configure_environment(a[0]);
            scheme_apply(nsreq, 1, a);
          }
        } else {
	  exit_val = 1;
	  p->error_buf = save;
	  break;
	}
	p->error_buf = save;
      } else if (fa->eval_kind[i] == mzcmd_MAIN) {
	Scheme_Thread * volatile p;
	p = scheme_get_current_thread();
	save = p->error_buf;
	p->error_buf = &newbuf;
	if (!scheme_setjmp(newbuf)) {
	  Scheme_Object *e, *a[2], *d2s, *nsi, *idb, *b, *cp;

          d2s = scheme_builtin_value("datum->syntax");
          a[0] = scheme_make_false();
          e = scheme_intern_symbol("main");
          a[1] = e;
          e = scheme_apply(d2s, 2, a);

          nsi = scheme_builtin_value("namespace-syntax-introduce");
          a[0] = e;
          e = scheme_apply(nsi, 1, a);

          /* Check that `main' is imported and/or defined: */
          idb = scheme_builtin_value("identifier-binding");
          a[0] = e;
          b = scheme_apply(idb, 1, a);
          if (b == scheme_make_false()) {
            if (!scheme_lookup_global(scheme_intern_symbol("main"), fa->global_env)) {
              scheme_signal_error("main: not defined or required into the top-level environment");
            }
          }

          e = scheme_make_pair(e, scheme_vector_to_list(fa->main_args));
          e = scheme_datum_to_kernel_stx(e);
          
	  e = scheme_eval_multi_with_prompt(e, fa->global_env);

          if (SAME_OBJ(e, SCHEME_MULTIPLE_VALUES)) {
            Scheme_Object **mv;
            int cnt;
            mv = p->ku.multiple.array;
            cnt = p->ku.multiple.count;
            scheme_detach_multple_array(mv);
            e = scheme_make_null();
            while (cnt--) {
              e = scheme_make_pair(mv[cnt], e);
            }
          } else {
            e = scheme_make_pair(e, scheme_make_null());
          }

          cp = scheme_get_param(scheme_current_config(), MZCONFIG_PRINT_HANDLER);
          
          while (SCHEME_PAIRP(e)) {
            a[0] = SCHEME_CAR(e);
            scheme_apply_multi(cp, 1, a);
            e = SCHEME_CDR(e);
          }
	} else {
	  exit_val = 1;
	  p->error_buf = save;
	  break;
	}
	p->error_buf = save;
      }
      did_config = 1;
    }
  }
#endif /* DONT_PARSE_COMMAND_LINE */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
  {
    Scheme_Object *f, *a[1];
    mz_jmp_buf * volatile save, newbuf;
    Scheme_Thread * volatile p;
    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      f = scheme_initialize(fa->global_env);
      a[0] = scheme_make_true();
      f = _scheme_apply_multi(f, 1, a);
      if (SAME_OBJ(f, SCHEME_MULTIPLE_VALUES)
	  && (scheme_multiple_count == 2)) {
	f = scheme_multiple_array[0];
	scheme_apply_multi_with_prompt(f, 0, NULL);
      }
    } else {
      exit_val = 1;
    }
    p->error_buf = save;
  }
#endif

#ifndef DONT_LOAD_INIT_FILE
  if (fa->a->use_repl && !fa->a->no_init_file) {
    char *filename;
    filename = GET_INIT_FILENAME(fa->global_env);
    if (filename) {
      filename = scheme_expand_filename(filename, -1, "startup", NULL, SCHEME_GUARD_FILE_EXISTS);
      if (scheme_file_exists(filename))
	scheme_load(filename);
    }
  }
#endif /* DONT_LOAD_INIT_FILE */

#ifndef DONT_RUN_REP
  if (fa->a->use_repl) {
    /* enter read-eval-print loop */
    mz_jmp_buf * volatile save, newbuf;
    Scheme_Thread * volatile p;
    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      repl(fa->global_env);
      exit_val = 0;
    } else {
      exit_val = 1;
#ifdef YIELD_BEFORE_EXIT
      fa->a->add_yield = 0;
#endif
    }
    p->error_buf = save;
  }
#endif /* DONT_RUN_REP */

#ifdef YIELD_BEFORE_EXIT
  if (fa->a->add_yield) {
    mz_jmp_buf * volatile save, newbuf;
    Scheme_Thread * volatile p;
    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      yield_indefinitely();
    }
    p->error_buf = save;
  }
#endif

  return exit_val;
}

static Scheme_Object *reverse_path_list(Scheme_Object *l, int rel_to_cwd)
{
  Scheme_Object *r, *path;

  r = scheme_make_null();
  while (SCHEME_PAIRP(l)) {
    path = SCHEME_CAR(l);
    if (rel_to_cwd)
      path = scheme_path_to_complete_path(path, NULL);
    r = scheme_make_pair(path, r);
    l = SCHEME_CDR(l);
  }
  
  return r;
}   

static int get_log_level(char *prog, char *real_switch, const char *envvar, const char *what, char *str)
{
  if (!strcmp(str, "none"))
    return 0;
  else if (!strcmp(str, "fatal"))
    return SCHEME_LOG_FATAL;
  else if (!strcmp(str, "error"))
    return SCHEME_LOG_ERROR;
  else if (!strcmp(str, "warning"))
    return SCHEME_LOG_WARNING;
  else if (!strcmp(str, "info"))
    return SCHEME_LOG_INFO;
  else if (!strcmp(str, "debug"))
    return SCHEME_LOG_DEBUG;

  PRINTF("%s: %s level %s%s%s must be one of the following:\n"
         "  none fatal error warning info or debug\n"
         " given: %s\n",
         prog, what, 
         real_switch ? "after " : "in ",
         real_switch ? real_switch : envvar,
         real_switch ? " switch" : " envrionment variable",
         str);
  return -1;
}

static int get_arg_log_level(char *prog, char *real_switch, const char *what, int argc, char **argv)
{
  if (argc < 2) {
    PRINTF("%s: missing %s level after %s switch\n", 
           prog, what, real_switch);
    return -1;
  }

  return get_log_level(prog, real_switch, NULL, what, argv[1]);
}

#ifdef USE_OSKIT_CONSOLE
/* Hack to disable normal input mode: */
int osk_not_console = 0;
#endif

#ifndef MZ_XFORM
# ifndef GC_CAN_IGNORE
#  define GC_CAN_IGNORE /**/
# endif
#endif

static int run_from_cmd_line(int argc, char *_argv[], 
			     Scheme_Env *(*mk_basic_env)(void),
			     int (*cont_run)(FinishArgs *f))
{
  GC_CAN_IGNORE char **argv = _argv;
  Scheme_Env *global_env;
  char *prog, *sprog = NULL;
  Scheme_Object *sch_argv,
                *collects_path = NULL, *collects_extra = NULL,
                *addon_dir = NULL;
  int i;
#ifndef DONT_PARSE_COMMAND_LINE
  char **evals_and_loads, *real_switch = NULL, specific_switch[2];
  int *eval_kind, num_enl;
  int no_more_switches = 0;
  int show_vers = 0;
#endif
#if !defined(DONT_RUN_REP) || !defined(DONT_PARSE_COMMAND_LINE)
  int use_repl = 0;
  int script_mode = 0;
#endif
#if !defined(DONT_LOAD_INIT_FILE) || !defined(DONT_PARSE_COMMAND_LINE)
  int no_init_file = 0;
#endif
#ifdef YIELD_BEFORE_EXIT
  int add_yield = 1;
#endif
#ifdef CMDLINE_STDIO_FLAG
  int alternate_rep = 0;
  int no_front = 0;
#endif
  char *init_lib = INITIAL_NAMESPACE_MODULE;
  int was_config_flag = 0, saw_nc_flag = 0;
  int no_compiled = 0;
  int init_ns = 0, no_init_ns = 0;
  int syslog_level = -1, stderr_level = -1;
  FinishArgs *fa;
  FinishArgsAtoms *fa_a;

#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_PROCESS_ATTACH, NULL);
#endif

  prog = argv[0];
  argv++;
  --argc;
  
#ifdef DOS_FILE_SYSTEM
  {
    /* For consistency, strip trailing spaces and dots, and make sure the .exe
       extension is present. */
    int l = strlen(prog);
    if ((l > 0) && ((prog[l-1] == ' ') || (prog[l-1] == '.'))) {
      char *s;
      while ((l > 0) && ((prog[l-1] == ' ') || (prog[l-1] == '.'))) {
	l--;
      }
      s  = (char *)scheme_malloc_atomic(l + 1);
      memcpy(s, prog, l);
      s[l] = 0;
      prog = s;
    }
    if (l <= 4 
	|| (prog[l - 4] != '.')
	|| (tolower(((unsigned char *)prog)[l - 3]) != 'e')
	|| (tolower(((unsigned char *)prog)[l - 2]) != 'x')
	|| (tolower(((unsigned char *)prog)[l - 1]) != 'e')) {
      char *s;
      s  = (char *)scheme_malloc_atomic(l + 4 + 1);
      memcpy(s, prog, l);
      memcpy(s + l, ".exe", 5);
      prog = s;
    }
  }
#endif
  
  /* If cmdline_exe_hack is changed, then we extract built-in
     arguments. */
  if (cmdline_exe_hack[0] != '[') {
    int n, i;
    long d;
    GC_CAN_IGNORE unsigned char *p;
    GC_CAN_IGNORE unsigned char *orig_p;
    char **argv2;

#ifdef DOS_FILE_SYSTEM
    if ((cmdline_exe_hack[0] == '?')
	|| (cmdline_exe_hack[0] == '*')) {
      /* This is how we make launchers in Windows.
	 The cmdline is appended to the end of the binary.
	 The long integer at cmdline_exe_hack[4] says
	 where the old end was, and cmdline_exe_hack[8]
	 says how long the cmdline string is. It might
	 be relative to the executable. */
      wchar_t *path;
      HANDLE fd;

      path = (wchar_t *)malloc(1024 * sizeof(wchar_t));
      GetModuleFileNameW(NULL, path, 1024);
      fd = CreateFileW(path, GENERIC_READ,
		       FILE_SHARE_READ | FILE_SHARE_WRITE,
		       NULL,
		       OPEN_EXISTING,
		       0,
		       NULL);
      if (fd == INVALID_HANDLE_VALUE)
	p = (unsigned char *)"\0\0\0";
      else {
	long start, len;
	DWORD got;
	start = *(long *)&cmdline_exe_hack[4];
	len = *(long *)&cmdline_exe_hack[8];
	p = (unsigned char *)malloc(len);
	SetFilePointer(fd, start, 0, FILE_BEGIN);
	ReadFile(fd, p, len, &got, NULL);
	CloseHandle(fd);
	if (got != len)
	  p = (unsigned char *)"\0\0\0";
	else if (cmdline_exe_hack[0] == '*') {
	  /* "*" means that the first item is argv[0] replacement: */
	  sprog = prog;
	  prog = (char *)p + 4;

	  if ((prog[0] == '\\')
	      || ((((prog[0] >= 'a') && (prog[0] <= 'z'))
		   || 	((prog[0] >= 'A') && (prog[0] <= 'Z')))
		  && (prog[1] == ':'))) {
	    /* Absolute path */
	  } else {
	    /* Make it absolute, relative to this executable */
	    int plen = strlen(prog);
	    int mlen, len;
	    char *s2, *p2;

	    /* UTF-8 encode path: */
	    for (len = 0; path[len]; len++) { }
	    mlen = scheme_utf8_encode((unsigned int *)path, 0, len,
				      NULL, 0,
				      1 /* UTF-16 */);
	    p2 = (char *)malloc(mlen + 1);
	    mlen = scheme_utf8_encode((unsigned int *)path, 0, len,
				      (unsigned char *)p2, 0,
				      1 /* UTF-16 */);

	    while (mlen && (p2[mlen - 1] != '\\')) {
	      mlen--;
	    }
	    s2 = (char *)malloc(mlen + plen + 1);
	    memcpy(s2, p2, mlen);
	    memcpy(s2 + mlen, prog, plen + 1);
	    prog = s2;
	  }

	  p += (p[0]
		+ (((long)p[1]) << 8)
		+ (((long)p[2]) << 16)
		+ (((long)p[3]) << 24)
		+ 4);
	}
      }
    } else
#endif
      p = (unsigned char *)cmdline_exe_hack + 1;

    /* Command line is encoded as a sequence of pascal-style strings;
       we use four whole bytes for the length, though, little-endian. */

    orig_p = p;

    n = 0;
    while (p[0] || p[1] || p[2] || p[3]) {
      n++;
      p += (p[0]
	    + (((long)p[1]) << 8)
	    + (((long)p[2]) << 16)
	    + (((long)p[3]) << 24)
	    + 4);
    }
    
    argv2 = (char **)malloc(sizeof(char *) * (argc + n));
    p = orig_p;
    for (i = 0; i < n; i++) {
      d = (p[0]
	   + (((long)p[1]) << 8)
	   + (((long)p[2]) << 16)
	   + (((long)p[3]) << 24));
      argv2[i] = (char *)p + 4;
      p += d + 4;
    }
    for (; i < n + argc; i++) {
      argv2[i] = argv[i - n];
    }
    argv = argv2;
    argc += n;
  }

#ifndef DONT_PARSE_COMMAND_LINE
  evals_and_loads = (char **)malloc(sizeof(char *) * argc);
  eval_kind = (int *)malloc(sizeof(int) * argc);
  num_enl = 0;

  while (!no_more_switches 
         && argc 
         && argv[0][0] == '-' 
         && argv[0][1]
         && !is_number_arg(argv[0] + 1)) {
    real_switch = argv[0];

    if (!strcmp("--help", argv[0]))
      argv[0] = "-h";
    else if (!strcmp("--eval", argv[0]))
      argv[0] = "-e";
    else if (!strcmp("--load", argv[0]))
      argv[0] = "-f";
    else if (!strcmp("--require", argv[0]))
      argv[0] = "-t";
    else if (!strcmp("--lib", argv[0]))
      argv[0] = "-l";
    else if (!strcmp("--script", argv[0]))
      argv[0] = "-r";
    else if (!strcmp("--require-script", argv[0]))
      argv[0] = "-u";
    else if (!strcmp("--main", argv[0]))
      argv[0] = "-m";
    else if (!strcmp("--name", argv[0]))
      argv[0] = "-N";
    else if (!strcmp("--no-compiled", argv[0]))
      argv[0] = "-c";
    else if (!strcmp("--no-lib", argv[0]))
      argv[0] = "-n";
    else if (!strcmp("--version", argv[0]))
      argv[0] = "-v";
    else if (!strcmp("--no-init-file", argv[0]))
      argv[0] = "-q";
    else if (!strcmp("--no-jit", argv[0]))
      argv[0] = "-j";
    else if (!strcmp("--no-delay", argv[0]))
      argv[0] = "-d";
    else if (!strcmp("--repl", argv[0]))
      argv[0] = "-i";
    else if (!strcmp("--binary", argv[0]))
      argv[0] = "-b";
    else if (!strcmp("--warn", argv[0]))
      argv[0] = "-W";
    else if (!strcmp("--syslog", argv[0]))
      argv[0] = "-L";
    else if (!strcmp("--collects", argv[0]))
      argv[0] = "-X";
    else if (!strcmp("--search", argv[0]))
      argv[0] = "-S";
    else if (!strcmp("--addon", argv[0]))
      argv[0] = "-A";
# ifdef CMDLINE_STDIO_FLAG
    else if (!strcmp("--stdio", argv[0]))
      argv[0] = "-z";
    else if (!strcmp("--back", argv[0]))
      argv[0] = "-G";
# endif
# ifdef YIELD_BEFORE_EXIT
    else if (!strcmp("--no-yield", argv[0]))
      argv[0] = "-V";
# endif
    
    if (!argv[0][1] || (argv[0][1] == '-' && argv[0][2])) {
      specific_switch[0] = 0;
      goto bad_switch;
    } else {
      GC_CAN_IGNORE char *str;
      char *se;
      for (str = argv[0] + 1; *str; str++) {
	switch (*str) {
	case 'h':
	  goto show_help;
	  break;
	case 'e':
	  if (argc < 2) {
	    PRINTF("%s: missing expression after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  eval_kind[num_enl++] = mzcmd_EVAL;
          init_ns = 1;
	  break;
	case 'X':
	  if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  collects_path = scheme_make_path(argv[0]);
          was_config_flag = 1;
	  break;
	case 'A':
	  if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  addon_dir = scheme_make_path(argv[0]);
          was_config_flag = 1;
	  break;
	case 'U':
	  scheme_set_ignore_user_paths(1);
          was_config_flag = 1;
	  break;
        case 'I':
          if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  init_lib = argv[0];
          was_config_flag = 1;
          break;
	case 'S':
	  if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  if (!collects_extra)
	    collects_extra = scheme_make_null();
          collects_extra = scheme_make_pair(scheme_make_path(argv[0]), collects_extra);
          was_config_flag = 1;
	  break;
        case 'c':
          no_compiled = 1;
          was_config_flag = 1;
          break;
	case 'm':
          evals_and_loads[num_enl] = "";
          eval_kind[num_enl++] = mzcmd_MAIN;
          init_ns = 1;
	  break;
	case 'r':
	  script_mode = 1;
	  no_more_switches = 1;
          if (argc > 1)
            sprog = argv[1];
	case 'f':
	  if (argc < 2) {
	    PRINTF("%s: missing file name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  eval_kind[num_enl++] = mzcmd_LOAD;
          init_ns = 1;
	  break;
	case 'u':
	  script_mode = 1;
	  no_more_switches = 1;
          if (argc > 1)
            sprog = argv[1];
	case 't':
	  if (argc < 2) {
	    PRINTF("%s: missing file name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  eval_kind[num_enl++] = mzcmd_REQUIRE_FILE;
          if (!init_ns)
            no_init_ns = 1;
	  break;
	case 'l':
	  if (argc < 2) {
	    PRINTF("%s: missing library name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  eval_kind[num_enl++] = mzcmd_REQUIRE_LIB;
          if (!init_ns)
            no_init_ns = 1;
	  break;
	case 'p':
	  if (argc < 2) {
	    PRINTF("%s: missing package name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  evals_and_loads[num_enl] = argv[0];
	  eval_kind[num_enl++] = mzcmd_REQUIRE_PLANET;
          if (!init_ns)
            no_init_ns = 1;
	  break;
	case 'k':
	  if (argc < 3) {
	    PRINTF("%s: missing %s after %s switch\n", 
		   prog, 
		   (argc < 2) ? "starting and ending offsets" : "ending offset",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_embedded_load(argv[0], argv[1]);
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  eval_kind[num_enl++] = mzcmd_EMBEDDED;
	  break;
	case 'N':
	  if (argc < 2) {
	    PRINTF("%s: missing name after %s switch\n", 
		   prog, 
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  sprog = argv[0];
          was_config_flag = 1;
	  break;
	case 'q':
	  no_init_file = 1;
          was_config_flag = 1;
	  break;
        case 'n':
          no_init_ns = 1;
          break;
	case 'v':
	  show_vers = 1;
	  break;
#ifdef YIELD_BEFORE_EXIT
	case 'V':
	  show_vers = 1;
	  add_yield = 0;
	  break;
#endif
	case 'i':
	  use_repl = 1;
          init_ns = 1;
	  break;
	case '-':
	  no_more_switches = 1;
          was_config_flag = 1;
	  break;
	case 'j':
	  scheme_set_startup_use_jit(0);
          was_config_flag = 1;
	  break;
	case 'd':
	  scheme_set_startup_load_on_demand(0);
          was_config_flag = 1;
	  break;
	case 'b':
	  scheme_set_binary_mode_stdio(1);
          was_config_flag = 1;
	  break;
#ifdef CMDLINE_STDIO_FLAG
       	case 'z':
	  alternate_rep = 1;
	  no_front = 1;
          was_config_flag = 1;
	  break;
	case 'K':
	  no_front = 1;
	  break;
#endif
#ifdef USE_OSKIT_CONSOLE
	case 'S':
	  osk_not_console = 1;
	  break;
#endif
        case 'W':
          stderr_level = get_arg_log_level(prog, real_switch, "stderr", argc, argv);
          if (stderr_level < 0)
            goto show_need_help;
          --argc;
          argv++;
          was_config_flag = 1;
          break;
	case 'L':
          syslog_level = get_arg_log_level(prog, real_switch, "syslog", argc, argv);
          if (syslog_level < 0)
            goto show_need_help;
          --argc;
          argv++;
          was_config_flag = 1;
          break;
	default:
          specific_switch[0] = *str;
          specific_switch[1] = 0;
	  goto bad_switch;
	}

        if (was_config_flag)
          was_config_flag = 0;
        else
          saw_nc_flag = 1;
      }
      argv++;
      --argc;
    }
  }

  if (!saw_nc_flag) {
    if (!argc) {
      /* No args => repl */
      use_repl = 1;
      init_ns = 1;
    } else {
      /* No switches => -u mode */
      script_mode = 1;
      no_more_switches = 1;
      sprog = argv[0];
      evals_and_loads[num_enl] = argv[0];
      eval_kind[num_enl++] = mzcmd_REQUIRE_FILE;
      argv++;
      --argc;
    }
  }

  if (use_repl) {
    show_vers = 1;
  }

  if (no_init_ns)
    init_ns = 0;

  if (show_vers) {
#ifndef RACKET_CMD_LINE  
    if (!use_repl
#ifdef CMDLINE_STDIO_FLAG
	|| alternate_rep
#endif
	)
#endif
      PRINTF("%s", BANNER);
#ifdef RACKET_CMD_LINE  
# ifdef DOS_FILE_SYSTEM
#  if !defined(FILES_HAVE_FDS)
#   if !defined(WINDOWS_FILE_HANDLES)
    PRINTF("Warning: read-eval-print-loop or read on a stream port may block threads.\n");
#   endif
#  endif
# endif
#endif

#if defined(USE_FD_PORTS) || defined(WINDOWS_FILE_HANDLES)
   fflush(stdout);
#endif
  }
#endif /* DONT_PARSE_COMMAND_LINE */

#ifdef GETENV_FUNCTION
  if (syslog_level < 0) {
    char *s;
    s = getenv("PLTSYSLOG");
    if (s) {
      syslog_level = get_log_level(prog, NULL, "PLTSYSLOG", "syslog", s);
    }
  }
  if (stderr_level < 0) {
    char *s;
    s = getenv("PLTSTDERR");
    if (s) {
      stderr_level = get_log_level(prog, NULL, "PLTSTDERR", "stderr", s);
    }
  }
#endif

  scheme_set_logging(syslog_level, stderr_level);

  global_env = mk_basic_env();

  sch_argv = scheme_make_vector(argc, NULL);
  for (i = 0; i < argc; i++) {
    Scheme_Object *so;
    so = scheme_make_locale_string(argv[i]);
    SCHEME_SET_CHAR_STRING_IMMUTABLE(so);
    SCHEME_VEC_ELS(sch_argv)[i] = so;
  }
  if (argc)
    SCHEME_SET_VECTOR_IMMUTABLE(sch_argv);
  scheme_set_param(scheme_current_config(), MZCONFIG_CMDLINE_ARGS, sch_argv);

  if (no_compiled)
    scheme_set_param(scheme_current_config(), MZCONFIG_USE_COMPILED_KIND, scheme_make_null());

  {	
    Scheme_Object *ps;
    scheme_set_exec_cmd(prog);
    if (!sprog)
      sprog = prog;
  
    ps = scheme_set_run_cmd(sprog);
  }

#ifndef NO_FILE_SYSTEM_UTILS
  /* Setup path for "addon" directory: */
  {
#ifdef GETENV_FUNCTION
    if (!addon_dir) {
      char *s;
      s = getenv("PLTADDONDIR");
      if (s) {
        s = scheme_expand_filename(s, -1, NULL, NULL, 0);
        if (s) addon_dir = scheme_make_path(s);
      }
    }
#endif
    if (addon_dir) scheme_set_addon_dir(addon_dir);
  }
  /* Setup path for "collects" collection directory: */
  {
    Scheme_Object *l, *r;
    int len, offset;

    if (!collects_path)
      collects_path = scheme_make_path(_coldir XFORM_OK_PLUS _coldir_offset);
    else
      collects_path = scheme_path_to_complete_path(collects_path, NULL);
    scheme_set_collects_path(collects_path);

    /* Make list of additional collection paths: */
    if (collects_extra)
      r = reverse_path_list(collects_extra, 1);
    else
      r = scheme_make_null();

    l = scheme_make_null();
    offset = _coldir_offset;
    while (1) {
      len = strlen(_coldir XFORM_OK_PLUS offset);
      offset += len + 1;
      if (!_coldir[offset])
        break;
      l = scheme_make_pair(scheme_make_path(_coldir XFORM_OK_PLUS offset),
                           l);
    }
    l = reverse_path_list(l, 0);

    scheme_init_collection_paths_post(global_env, l, r);
  }
#endif /* NO_FILE_SYSTEM_UTILS */

  scheme_seal_parameters();

  fa_a = (FinishArgsAtoms *)scheme_malloc_atomic(sizeof(FinishArgsAtoms));
  fa = (FinishArgs *)scheme_malloc(sizeof(FinishArgs));
  fa->a = fa_a;
  fa->a->init_ns = init_ns;
#ifndef DONT_PARSE_COMMAND_LINE
  fa->evals_and_loads = evals_and_loads;
  fa->eval_kind = eval_kind;
  fa->a->num_enl = num_enl;
  fa->main_args = sch_argv;
#endif  
#ifndef DONT_LOAD_INIT_FILE
  fa->a->no_init_file = no_init_file;
#endif
#ifndef DONT_RUN_REP
  fa->a->use_repl = use_repl;
  fa->a->script_mode = script_mode;
#endif
#ifdef YIELD_BEFORE_EXIT
  fa->a->add_yield = add_yield;
#endif
#ifdef CMDLINE_STDIO_FLAG
  fa->a->alternate_rep = alternate_rep;
  fa->a->no_front = no_front;
#endif
  fa->init_lib = init_lib;
  fa->global_env = global_env;

  scheme_set_can_break(1);

  return cont_run(fa);

#ifndef DONT_PARSE_COMMAND_LINE
 show_help:
  prog =("%s"
         PROGRAM_LC " [<option> ...] <argument> ...\n"
# ifndef RACKET_CMD_LINE
#  ifdef wx_x
	 " X configuration options (must precede all other options):\n"
	 "  -display <display>, -geometry <geometry>, -fn <font>, -font <font>,\n"
         "  -bg <color>, -background <color>, -fg <color>, -foreground <color>,\n"
	 "  -iconic, -name <name>, -rv, -reverse, +rv, -selectionTimeout <timeout>,\n"
	 "  -synchronous, -title <title>, -xnllanguage <language>, -xrm <file>\n"
#  endif
# endif
	 " File and expression options:\n"
	 "  -e <exprs>, --eval <exprs> : Evaluate <exprs>, prints results\n"
	 "  -f <file>, --load <file> : Like -e '(load \"<file>\")' without printing\n"
	 "  -t <file>, --require <file> : Like -e '(require (file \"<file>\"))'\n"
	 "  -l <path>, --lib <path> : Like -e '(require (lib \"<path>\"))'\n"
	 "  -p <package> : Like -e '(require (planet \"<package>\")'\n"
	 "  -r <file>, --script <file> : Same as -f <file> -N <file> --\n" 
	 "  -u <file>, --require-script <file> : Same as -t <file> -N <file> --\n" 
	 "  -k <n> <m> : Load executable-embedded code from file offset <n> to <m>\n"
	 "  -m, --main : Call `main' with command-line arguments, print results\n"
	 " Interaction options:\n"
         "  -i, --repl : Run interactive read-eval-print loop; implies -v\n"
         "  -n, --no-lib : Skip `(require (lib \"<init-lib>\"))' for -i/-e/-f/-r\n"
	 "  -v, --version : Show version\n"
# ifdef CMDLINE_STDIO_FLAG
	 "  -K, --back : Don't bring application to the foreground (Mac OS X)\n"
# endif
# ifdef YIELD_BEFORE_EXIT
	 "  -V, --no-yield : Don't `(yield 'wait)'\n"
# endif
	 " Configuration options:\n"
         "  -c, --no-compiled : Disable loading of compiled files\n"
	 "  -q, --no-init-file : Skip load of " INIT_FILENAME " for -i\n"
# ifdef CMDLINE_STDIO_FLAG
	 "  -z, --text-repl : Use text read-eval-print loop for -i\n"
# endif
         "  -I <path> : Set <init-lib> to <path>\n"
	 "  -X <dir>, --collects <dir> : Main collects at <dir>\n"
	 "  -S <dir>, --search <dir> : More collects at <dir> (after main collects)\n"
	 "  -A <dir>, --addon <dir> : Addon directory at <dir>\n"
	 "  -U, --no-user-path : Ignore user-specific collects, etc.\n"
	 "  -N <file>, --name <file> : Sets `(find-system-path 'run-file)' to <file>\n"
# ifdef MZ_USE_JIT
	 "  -j, --no-jit : Disable the just-in-time compiler\n"
# else
	 "  -j, --no-jit : No effect, since the just-in-time compiler is unavailable\n"
# endif
         "  -d, --no-delay: Disable on-demand loading of syntax and code\n"
	 "  -b, --binary : Read stdin and write stdout/stderr in binary mode\n"
	 "  -W <level>, --warn <level> : Set stderr logging to <level>\n"
	 "  -L <level>, --syslog <level> : Set syslog logging to <level>\n"
	 " Meta options:\n"
	 "  -- : No argument following this switch is used as a switch\n"
	 "  -h, --help : Show this information and exits, ignoring other options\n"
         "Default options:\n"
         " If only configuration options are provided, -i is added\n"
         " If only configuration options are before the first argument, -u is added\n"
         " If -t/-l/-p/-u apears before the first -i/-e/-f/-r, -n is added\n"
         " <init-lib> defaults to " INITIAL_NAMESPACE_MODULE "\n"
         "Switch syntax:\n"
	 " Multiple single-letter switches can be collapsed, with arguments placed\n"
	 "   after the collapsed switches; the first collapsed switch cannot be --\n"
	 " Example: `-ifve file expr' is the same as `-i -f file -v -e expr'\n"
         "Start-up sequence:\n"
         " 1. Set `current-library-collection-paths'\n"
         " 2. Require `(lib \"<init-lib>\")' [when -i/-e/-f/-r, unless -n]\n"
         " 3. Evaluate/load expressions/files in order, until first error\n"
         " 4. Load \"" INIT_FILENAME "\" [when -i]\n"
         " 5. Run read-eval-print loop [when -i]\n"
# ifdef YIELD_BEFORE_EXIT
         " 6. Run `(yield 'wait)' [unless -V]\n"
# endif
	 );
  PRINTF(prog, BANNER);
#if defined(WINDOWS_FILE_HANDLES)
  fflush(stdout);
#endif
  return 0;
 bad_switch:
  if (specific_switch[0] && real_switch[2]) {
    PRINTF("%s: bad switch: -%s within: %s\n", prog, specific_switch, real_switch);
  } else {
    PRINTF("%s: bad switch: %s\n", prog, real_switch);
  }
 show_need_help:
  PRINTF("Use the --help or -h flag for help.\n");
#if defined(DETECT_WIN32_CONSOLE_STDIN)
  fflush(stdout);
#endif
  return 1;
#endif
}

#if defined(OS_X) && defined(MZ_PRECISE_GC) && defined(EXTRA_EXCEPTION_STUBS)

/* These declarations avoid linker problems when using
   -mmacosx-version-min. See gc2/vm_osx.c for details. */

START_XFORM_SKIP;

#include <mach/mach.h>
#include <mach/mach_error.h>

# ifdef __cplusplus
extern "C" {
# endif

extern kern_return_t GC_catch_exception_raise_state(mach_port_t port,
                                                    exception_type_t exception_type,
                                                    exception_data_t exception_data,
                                                    mach_msg_type_number_t data_cnt,
                                                    thread_state_flavor_t *flavor,
                                                    thread_state_t in_state,
                                                    mach_msg_type_number_t is_cnt,
                                                    thread_state_t out_state,
                                                    mach_msg_type_number_t os_cnt);
extern kern_return_t GC_catch_exception_raise_state_identitity
  (mach_port_t port,  mach_port_t thread_port, mach_port_t task_port,
   exception_type_t exception_type, exception_data_t exception_data,
   mach_msg_type_number_t data_count, thread_state_flavor_t *state_flavor,
   thread_state_t in_state, mach_msg_type_number_t in_state_count,
   thread_state_t out_state, mach_msg_type_number_t out_state_count);
extern kern_return_t GC_catch_exception_raise(mach_port_t port,
                                              mach_port_t thread_port,
                                              mach_port_t task_port,
                                              exception_type_t exception_type,
                                              exception_data_t exception_data,
                                              mach_msg_type_number_t data_count);

kern_return_t catch_exception_raise_state(mach_port_t port,
					  exception_type_t exception_type,
					  exception_data_t exception_data,
					  mach_msg_type_number_t data_cnt,
					  thread_state_flavor_t *flavor,
					  thread_state_t in_state,
					  mach_msg_type_number_t is_cnt,
					  thread_state_t out_state,
					  mach_msg_type_number_t os_cnt)
{
  return GC_catch_exception_raise_state(port, exception_type, exception_data,
                                        data_cnt, flavor,
                                        in_state, is_cnt,
                                        out_state, os_cnt);
}

kern_return_t catch_exception_raise_state_identitity
  (mach_port_t port,  mach_port_t thread_port, mach_port_t task_port,
   exception_type_t exception_type, exception_data_t exception_data,
   mach_msg_type_number_t data_count, thread_state_flavor_t *state_flavor,
   thread_state_t in_state, mach_msg_type_number_t in_state_count,
   thread_state_t out_state, mach_msg_type_number_t out_state_count)
{
  return GC_catch_exception_raise_state_identitity(port, thread_port, task_port,
                                                   exception_type, exception_data,
                                                   data_count, state_flavor,
                                                   in_state, in_state_count,
                                                   out_state, out_state_count);
}

kern_return_t catch_exception_raise(mach_port_t port,
				    mach_port_t thread_port,
				    mach_port_t task_port,
				    exception_type_t exception_type,
				    exception_data_t exception_data,
				    mach_msg_type_number_t data_count)
{
  return GC_catch_exception_raise(port, thread_port, task_port,
                                  exception_type, exception_data, data_count);
}

# ifdef __cplusplus
};
# endif
END_XFORM_SKIP;
#endif
