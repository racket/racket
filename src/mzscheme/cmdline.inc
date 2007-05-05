
/***************************************************************/
/* This command-line parser is used by both MzScheme and MrEd. */
/***************************************************************/

#define SDESC "Set! works on undefined identifiers"

char *cmdline_exe_hack = "[Replace me for EXE hack                                       ]";

#ifdef MZ_PRECISE_GC
# define GC_PRECISION_TYPE "3"
#else
# define GC_PRECISION_TYPE "c"
#endif
char *binary_type_hack = "bINARy tYPe:" INITIAL_BIN_TYPE GC_PRECISION_TYPE;
/* The format of bINARy tYPe is e?[zr]i[3c].
   e indicates a starter executable
   z/r indicates MzScheme or MrEd
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

static char *protect_quote_backslash(const char *file)
{
  int i, c;
  
  for (i = c = 0; file[i]; i++) {
    if ((file[i] == '"') || (file[i] == '\\'))
      c++;
  }

  if (c) {
    char *s;
  
    s = (char *)malloc(i + c + 1);

    for (i = c = 0; file[i]; i++) {
      if ((file[i] == '"') || (file[i] == '\\'))
	s[c++] = '\\';
      s[c++] = file[i];
    }
    s[c] = 0;

    return s;
  } else
    return (char *)file;
}

static char *check_script_runner(char *prog)
{
  /* Check whether the program name is "...scheme-...": */
  int i;

#ifdef UNIX_FILE_SYSTEM
# define IS_A_SEP(x) ((x) == '/')
#endif
#ifdef DOS_FILE_SYSTEM
# define IS_A_SEP(x) (((x) == '/') || ((x) == '\\'))
#endif
#ifdef MAC_FILE_SYSTEM
# define IS_A_SEP(x) ((x) == ':')
#endif

  for (i = 0; prog[i]; i++) {
    if ((prog[i] == 's')
	&& (prog[i+1] == 'c')
	&& (prog[i+2] == 'h')
	&& (prog[i+3] == 'e')
	&& (prog[i+4] == 'm')
	&& (prog[i+5] == 'e')
	&& (prog[i+6] == '-')) {
      /* Maybe. Check directory separators. */
      if (!i || IS_A_SEP(prog[i-1])) {
	int j;
	for (j = i + 7; prog[j]; j++) {
	  if (IS_A_SEP(prog[j]))
	    break;
	}
	if (!prog[j]) {
	  /* Yes, this is a script runner. */
	  char *lang;
#ifdef DOS_FILE_SYSTEM
	  /* Remove ".exe": */
	  j -= 4;
#endif

	  i += 7;
	  j -= i;
	  lang = (char *)malloc(j + 55);
	  memcpy(lang, "(require (lib \"init.ss\" \"script-lang\" \"", 39);
	  memcpy(lang + 39, prog + i, j);
	  memcpy(lang + 39 + j, "\"))", 4); /* gets null term */

	  return lang;
	}
      }
    }
  }

  return NULL;
}

static char *make_load_cd(char *file)
{
  char *s;

  file = protect_quote_backslash(file);
  
  s = (char *)malloc(strlen(file) + 13);
  strcpy(s, "(load/cd \"");
  strcat(s, file);
  strcat(s, "\")");
  return s;
}

static char *make_require(char *file)
{
  char *s;
  
  file = protect_quote_backslash(file);
  
  s = (char *)malloc(strlen(file) + 20);
  strcpy(s, "(require (file \"");
  strcat(s, file);
  strcat(s, "\"))");
  return s;
}

static char *make_require_lib(const char *file, const char *coll)
{
  char *s;
  
  file = protect_quote_backslash(file);
  coll = protect_quote_backslash(coll);
  
  s = (char *)malloc(strlen(file) + strlen(coll) + 31);
  strcpy(s, "(require (lib \"");
  strcat(s, file);
  strcat(s, "\" \"");
  strcat(s, coll);
  strcat(s, "\"))");
  return s;
}

static char *make_require_coll(const char *coll)
{
  char *s;

  s = (char *)malloc(strlen(coll) + 4);
  strcpy(s, coll);
  strcat(s, ".ss");
  return make_require_lib(s, coll);
}

static char *make_require_planet(const char *file, const char *file_sfx, 
                                 const char *user, 
                                 const char *pkg, char *pkg_sfx)
{
  char *s;
  
  pkg = protect_quote_backslash(pkg);
  user = protect_quote_backslash(user);
  
  s = (char *)malloc(strlen(file) 
                     + strlen(file_sfx) 
                     + strlen(user) 
                     + strlen(pkg) 
                     + strlen(pkg_sfx) + 46);
  strcpy(s, "(require (planet \"");
  strcat(s, file);
  strcat(s, file_sfx);
  strcat(s, "\" (\"");
  strcat(s, user);
  strcat(s, "\" \"");
  strcat(s, pkg);
  strcat(s, pkg_sfx);
  strcat(s, "\")))");
  return s;
}

static char *make_embedded_load(const char *start, const char *end)
{
  char *s;

  s = (char *)malloc((2 * strlen(start)) + strlen(end) + 512);
  
  sprintf(s, "%s %s %s %s %s %s",
	  "(let* ([sp (find-system-path 'exec-file)] [exe (find-executable-path sp #f)]"
                   "[s (with-input-from-file exe (lambda () "
	                "(file-position (current-input-port)", start, ") "
                                         "(read-bytes (- ", end, start, "))))]"
                   "[p (open-input-bytes s)])"
	    "(when (and (> (bytes-length s) 2)"
                           "(= (bytes-ref s 0) (char->integer #\\#))"
                           "(= (bytes-ref s 1) (char->integer #\\!)))"
               "(read-bytes-line p))"
	    "(let loop () (let ([e (parameterize ([read-accept-compiled #t]) (read p))])"
 			     "(unless (eof-object? e) (eval e) (loop)))))");

  return s;
}
#endif

#define mzcmd_EVAL 0
#define mzcmd_LOAD 1
#define mzcmd_MAIN 2

/* To avoid having to create a specific mark procedure for
   prcise GC, split argument information into purely atomic
   and purely non-atomic records. */

typedef struct {
  int start_with_req;
#ifndef DONT_PARSE_COMMAND_LINE
  int num_enl;
#endif  
#ifndef DONT_LOAD_INIT_FILE
  int no_init_file;
#endif
#ifndef DONT_RUN_REP
  int no_rep;
  int script_mode;
#endif
#ifdef VERSION_YIELD_FLAG
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
} FinishArgs;

typedef void (*Repl_Proc)(Scheme_Env *);

static int finish_cmd_line_run(FinishArgs *fa, Repl_Proc repl)
{
  volatile int exit_val = 0;

  if (fa->a->start_with_req) {
    scheme_eval_string("(require mzscheme)", fa->global_env);    
  }

#ifndef DONT_LOAD_INIT_FILE
  if (!fa->a->no_init_file) {
    char *filename;
    filename = GET_INIT_FILENAME(fa->global_env);
    if (filename) {
      filename = scheme_expand_filename(filename, -1, "startup", NULL, SCHEME_GUARD_FILE_EXISTS);
      if (scheme_file_exists(filename))
	scheme_load(filename);
    }
  }
#endif /* DONT_LOAD_INIT_FILE */

#ifndef DONT_PARSE_COMMAND_LINE
  {
    volatile int i;
    mz_jmp_buf * volatile save, newbuf;

    for (i = 0; i < fa->a->num_enl; i++) {
      if (fa->eval_kind[i] == mzcmd_LOAD) {
	if (!scheme_load(fa->evals_and_loads[i])) {
	  exit_val = 1;
	  break;
	}
      } else if (fa->eval_kind[i] == mzcmd_EVAL) {
	Scheme_Thread * volatile p;
	p = scheme_get_current_thread();
	save = p->error_buf;
	p->error_buf = &newbuf;
	if (!scheme_setjmp(newbuf))
	  scheme_eval_string_all_with_prompt(fa->evals_and_loads[i], fa->global_env, 0);
	else {
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
	  Scheme_Object *a[1], *m, *fn;
	  
	  m = scheme_eval_string_with_prompt("main", fa->global_env);
	  fn = scheme_make_locale_string(fa->evals_and_loads[i]);
	  SCHEME_SET_CHAR_STRING_IMMUTABLE(fn);
	  a[0] = scheme_make_pair(fn, scheme_vector_to_list(fa->main_args));
	  (void)scheme_apply_multi_with_prompt(m, 1, a);
	} else {
	  exit_val = 1;
	  p->error_buf = save;
	  break;
	}
	p->error_buf = save;
      }
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

#ifndef DONT_RUN_REP
  if (!fa->a->no_rep && !fa->a->script_mode) {
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
#ifdef VERSION_YIELD_FLAG
      fa->a->add_yield = 0;
#endif
    }
    p->error_buf = save;
  }
#endif /* DONT_RUN_REP */

#ifdef VERSION_YIELD_FLAG
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

#ifndef NO_FILE_SYSTEM_UTILS
static void init_collection_paths(Scheme_Env *global_env, Scheme_Object *extra_dirs)
{		
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {
    Scheme_Object *clcp, *flcp, *a[1];

    clcp = scheme_builtin_value("current-library-collection-paths");
    flcp = scheme_builtin_value("find-library-collection-paths");

    if (clcp && flcp) {
      a[0] = extra_dirs;
      a[0] = _scheme_apply(flcp, 1, a);
      _scheme_apply(clcp, 1, a);
    }
  }
  p->error_buf = save;
}
#endif

#ifndef MZSCHEME_CMD_LINE  
static void init_mred(Scheme_Env *global_env)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;
  if (!scheme_setjmp(newbuf)) {
    scheme_eval_string("(require (lib \"class.ss\"))", 
		       global_env);
    scheme_eval_string("(require (lib \"mred.ss\" \"mred\"))", 
		       global_env);
    scheme_eval_string("(current-load text-editor-load-handler)", 
		       global_env);
  }
  p->error_buf = save;
}
#endif

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
  Scheme_Object *sch_argv, *collects_path = NULL, *collects_extra = NULL;
  int i;
#ifndef DONT_PARSE_COMMAND_LINE
  char **evals_and_loads, *real_switch = NULL, *runner;
  int *eval_kind, num_enl;
  int no_more_switches = 0;
  int mute_banner = 0;
  int no_argv = 0;
  int start_with_req = 0;
#endif
#if !defined(DONT_RUN_REP) || !defined(DONT_PARSE_COMMAND_LINE)
  int no_rep = 0;
  int script_mode = 0;
#endif
#if !defined(MZSCHEME_CMD_LINE)
  int skip_init_mred = 0;
#endif
#if !defined(DONT_LOAD_INIT_FILE) || !defined(DONT_PARSE_COMMAND_LINE)
  int no_init_file = 0;
#endif
#ifdef VERSION_YIELD_FLAG
  int add_yield = 1;
#endif
#ifdef CMDLINE_STDIO_FLAG
  int alternate_rep = 0;
  int no_front = 0;
#endif
  int no_lib_path = 0;
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

  runner = check_script_runner(prog);
  if (runner) {
    if (!argc) {
      PRINTF("%s: missing script filename to load\n",
	     prog);
      return 1;
    }

    evals_and_loads[0] = runner;
    eval_kind[0] = mzcmd_EVAL;
    evals_and_loads[1] = argv[0];
    eval_kind[1] = mzcmd_LOAD;
    evals_and_loads[2] = argv[0];
    eval_kind[2] = mzcmd_MAIN;
    num_enl = 3;
    script_mode = 1;
    no_more_switches = 1;
    no_init_file = 1;
    no_argv = 1;
    argv++;
    --argc;
  }

  while (!no_more_switches && argc && argv[0][0] == '-' && !is_number_arg(argv[0] + 1)) {
    real_switch = argv[0];

    if (!strcmp("--help", argv[0]))
      argv[0] = "-h";
    else if (!strcmp("--eval", argv[0]))
      argv[0] = "-e";
    else if (!strcmp("--load", argv[0]))
      argv[0] = "-f";
    else if (!strcmp("--load-cd", argv[0]))
      argv[0] = "-d";
    else if (!strcmp("--require", argv[0]))
      argv[0] = "-t";
    else if (!strcmp("--Load", argv[0]))
      argv[0] = "-F";
    else if (!strcmp("--Load-cd", argv[0]))
      argv[0] = "-D";
    else if (!strcmp("--Require", argv[0]))
      argv[0] = "-T";
    else if (!strcmp("--mzlib", argv[0]))
      argv[0] = "-l";
    else if (!strcmp("--case-sens", argv[0]))
      argv[0] = "-g";
    else if (!strcmp("--case-insens", argv[0]))
      argv[0] = "-G";
    else if (!strcmp("--set-undef", argv[0]))
      argv[0] = "-s";
    else if (!strcmp("--script", argv[0]))
      argv[0] = "-r";
    else if (!strcmp("--script-cd", argv[0]))
      argv[0] = "-i";
    else if (!strcmp("--require-script", argv[0]))
      argv[0] = "-u";
    else if (!strcmp("--main", argv[0]))
      argv[0] = "-I";
    else if (!strcmp("--name", argv[0]))
      argv[0] = "-N";
    else if (!strcmp("--no-lib-path", argv[0]))
      argv[0] = "-x";
    else if (!strcmp("--version", argv[0]))
      argv[0] = "-v";
    else if (!strcmp("--no-init-file", argv[0]))
      argv[0] = "-q";
    else if (!strcmp("--no-jit", argv[0]))
      argv[0] = "-j";
    else if (!strcmp("--no-argv", argv[0]))
      argv[0] = "-A";
    else if (!strcmp("--mute-banner", argv[0]))
      argv[0] = "-m";
    else if (!strcmp("--awk", argv[0]))
      argv[0] = "-w";
    else if (!strcmp("--binary", argv[0]))
      argv[0] = "-b";
    else if (!strcmp("--collects", argv[0]))
      argv[0] = "-X";
    else if (!strcmp("--search", argv[0]))
      argv[0] = "-S";
    else if (!strcmp("--prim", argv[0]))
      argv[0] = "-Q";
# ifndef MZSCHEME_CMD_LINE
    else if (!strcmp("--nogui", argv[0]))
      argv[0] = "-Z";
# endif
# ifdef CMDLINE_STDIO_FLAG
    else if (!strcmp("--stdio", argv[0]))
      argv[0] = "-z";
    else if (!strcmp("--back", argv[0]))
      argv[0] = "-G";
# endif
# ifdef VERSION_YIELD_FLAG
    else if (!strcmp("--yield", argv[0]))
      argv[0] = "-V";
# endif
    else if (!strcmp("--restore", argv[0])) {
      PRINTF("--restore or -R<file> must be the first (and only) switch\n");
      goto show_need_help;
    }
    
    if (!argv[0][1] || (argv[0][1] == '-' && argv[0][2])) {
      goto bad_switch;
    } else {
      GC_CAN_IGNORE char *str;
      char *se;
      for (str = argv[0] + 1; *str; str++) {
	switch (*str) {
	case 'h':
	  goto show_help;
	  break;
	case 'g':
	  scheme_set_case_sensitive(1);
	  break;
	case 'G':
	  scheme_set_case_sensitive(0);
	  break;
	case 's':
	  scheme_set_allow_set_undefined(1);
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
	  break;
	case 'U':
	  scheme_set_ignore_user_paths(1);
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
	  collects_extra = scheme_make_pair(scheme_make_path(argv[0]),
					    collects_extra);
	  break;
	case 'x':
	  no_lib_path = 1;
	  break;
	case 'C':
	case 'r':
	  script_mode = 1;
	  no_more_switches = 1;
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
	  if (*str == 'C') {
	    evals_and_loads[num_enl] = argv[0];
	    eval_kind[num_enl++] = mzcmd_MAIN;
	  }
	  break;
	case 'i':
	  script_mode = 1;
	  no_more_switches = 1;
	case 'd':
	  if (argc < 2) {
	    PRINTF("%s: missing file name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_load_cd(argv[0]);
	  evals_and_loads[num_enl] = se;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'u':
	  script_mode = 1;
	  no_more_switches = 1;
	case 't':
	  if (argc < 2) {
	    PRINTF("%s: missing file name after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require(argv[0]);
	  evals_and_loads[num_enl] = se;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'F':
	  while (argc > 1) {
	    argv++;
	    --argc;
	    evals_and_loads[num_enl] = argv[0];
	    eval_kind[num_enl++] = mzcmd_LOAD;
	  }
	  break;
	case 'D':
	  while (argc > 1) {
	    argv++;
	    --argc;
	    se = make_load_cd(argv[0]);
	    evals_and_loads[num_enl] = se;
	    eval_kind[num_enl++] = mzcmd_EVAL;
	  }
	  break;
	case 'T':
	  while (argc > 1) {
	    argv++;
	    --argc;
	    se = make_require(argv[0]);
	    evals_and_loads[num_enl] = se;
	    eval_kind[num_enl++] = mzcmd_EVAL;
	  }
	  break;
	case 'l':
	  if (argc < 2) {
	    PRINTF("%s: missing file after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_lib(argv[0], "mzlib");
	  evals_and_loads[num_enl] = se;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'L':
	  if (argc < 3) {
	    PRINTF("%s: missing %s after %s switch\n", 
		   prog, 
		   (argc < 2) ? "file and collection" : "collection",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_lib(argv[0], argv[1]);
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'M':
	  if (argc < 2) {
	    PRINTF("%s: missing collection after %s switch\n", 
		   prog, 
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_coll(argv[0]);
	  evals_and_loads[num_enl] = se;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'p':
	  if (argc < 4) {
	    PRINTF("%s: missing %s after %s switch\n", 
		   prog, 
		   ((argc > 2) 
                    ? "package"
                    : ((argc > 1) 
                       ? "user and package" 
                       : "file, user, and package")),
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_planet(argv[0], "", argv[1], argv[2], "");
	  evals_and_loads[num_enl] = se;
	  argv += 2;
	  argc -= 2;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
	case 'P':
	  if (argc < 3) {
	    PRINTF("%s: missing %s after %s switch\n", 
		   prog, 
		   (argc < 2) ? "package/file base name and user" : "package",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_require_planet(argv[0], ".ss", argv[1], argv[0], ".plt");
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  eval_kind[num_enl++] = mzcmd_EVAL;
	  break;
        case 'Q':
          start_with_req = 1;
          break;
	case 'w':
	  se = make_require_lib("awk.ss", "mzlib");
	  evals_and_loads[num_enl] = se;
	  eval_kind[num_enl++] = mzcmd_EVAL;
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
	  eval_kind[num_enl++] = mzcmd_EVAL;
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
	  break;
	case 'A':
	  no_argv = 1;
	  break;
	case 'q':
	  no_init_file = 1;
	  break;
	case 'v':
	  no_rep = 1;
	  break;
#ifdef VERSION_YIELD_FLAG
	case 'V':
	  no_rep = 1;
	  add_yield = 0;
	  break;
#endif
	case 'm':
	  mute_banner = 1;
	  break;
	case '-':
	  no_more_switches = 1;
	  break;
	case 'j':
	  scheme_set_startup_use_jit(0);
	  break;
	case 'b':
	  scheme_set_binary_mode_stdio(1);
	  break;
#ifndef MZSCHEME_CMD_LINE
	case 'Z':
	  skip_init_mred = 1;
	  break;
#endif
#ifdef CMDLINE_STDIO_FLAG
       	case 'z':
	  alternate_rep = 1;
	  no_front = 1;
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
	case 'R':
	  PRINTF("--restore or -R<file> must be the first (and only) switch\n");
	  goto show_need_help;
	  break;
	default:
	  goto bad_switch;
	}
      }
    }
    argv++;
    --argc;
  }

  if (!script_mode && !mute_banner) {
#ifndef MZSCHEME_CMD_LINE  
    if (no_rep
#ifdef CMDLINE_STDIO_FLAG
	|| alternate_rep
#endif
	)
#endif
      PRINTF(BANNER);
#ifdef MZSCHEME_CMD_LINE  
    if (scheme_get_allow_set_undefined())
      PRINTF(SDESC ".\n");
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

#ifndef DONT_PARSE_COMMAND_LINE
  if (!no_argv)
#endif
    scheme_add_global("argv", sch_argv, global_env);
  {	
    Scheme_Object *ps;
    scheme_set_exec_cmd(prog);
    if (!sprog)
      sprog = prog;
    ps = scheme_set_run_cmd(sprog);
#ifndef DONT_PARSE_COMMAND_LINE
    if (!no_argv)
#endif
      scheme_add_global("program", ps, global_env);
  }

#ifndef NO_FILE_SYSTEM_UTILS
  /* Setup path for "collects" collection directory: */
  if (!no_lib_path) {
    Scheme_Object *l, *r;
    int len, offset;

    if (!collects_path)
      collects_path = scheme_make_path(_coldir XFORM_OK_PLUS _coldir_offset);
    scheme_set_collects_path(collects_path);

    /* Make list of additional collection paths: */
    if (collects_extra) {
      l = collects_extra;
    } else {
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
    }
    /* Reverse list */
    r = scheme_make_null();
    while (SCHEME_PAIRP(l)) {
      r = scheme_make_pair(SCHEME_CAR(l), r);
      l = SCHEME_CDR(l);
    }
    
    init_collection_paths(global_env, r);
  }
#endif /* NO_FILE_SYSTEM_UTILS */

#ifndef MZSCHEME_CMD_LINE
  if (!skip_init_mred)
    init_mred(global_env);
#endif

  fa_a = (FinishArgsAtoms *)scheme_malloc_atomic(sizeof(FinishArgsAtoms));
  fa = (FinishArgs *)scheme_malloc(sizeof(FinishArgs));
  fa->a = fa_a;
  fa->a->start_with_req = start_with_req;
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
  fa->a->no_rep = no_rep;
  fa->a->script_mode = script_mode;
#endif
#ifdef VERSION_YIELD_FLAG
  fa->a->add_yield = add_yield;
#endif
#ifdef CMDLINE_STDIO_FLAG
  fa->a->alternate_rep = alternate_rep;
  fa->a->no_front = no_front;
#endif
  fa->global_env = global_env;

  scheme_set_can_break(1);

  return cont_run(fa);

#ifndef DONT_PARSE_COMMAND_LINE
 show_help:
  prog =("%s"
         PROGRAM_LC " [<option> ...] <argument> ...\n"
# ifndef MZSCHEME_CMD_LINE
#  ifdef wx_x
	 " X options (must precede all other options):\n"
	 "  -display <display>, -geometry <geometry>, -fn <font>, -font <font>,\n"
         "  -bg <color>, -background <color>, -fg <color>, -foreground <color>,\n"
	 "  -iconic, -name <name>, -rv, -reverse, +rv, -selectionTimeout <timeout>,\n"
	 "  -synchronous, -title <title>, -xnllanguage <language>, -xrm <file>\n"
#  endif
# endif
	 " Startup file and expression options:\n"
	 "  -e <expr>, --eval <expr> : Evaluates <expr> after " PROGRAM " starts\n"
	 "  -f <file>, --load <file> : Loads <file> after " PROGRAM " starts\n"
	 "  -d <file>, --load-cd <file> : Load/cds <file> after " PROGRAM " starts\n"
	 "  -t <file>, --require <file> : Requires <file> after " PROGRAM " starts\n"
	 "  -F, --Load : Loads all remaining arguments after " PROGRAM " starts\n"
	 "  -D, --Load-cd : Load/cds all remaining arguments after " PROGRAM " starts\n"
	 "  -T, --Require : Requires all remaining arguments after " PROGRAM " starts\n"
	 "  -l <file>, --mzlib <file> : Same as -e '(require (lib \"<file>\"))'\n"
	 "  -L <file> <coll> : Same as -e '(require (lib \"<file>\" \"<coll>\"))'\n"
	 "  -M <coll> : Same as -e '(require (lib \"<coll>.ss\" \"<coll>\"))'\n"
	 "  -p <fl> <u> <pkg> : Same as -e '(require (planet \"<fl>\" (\"<u>\" \"<pkg>\"))'\n"
	 "  -P <nm> <u> : Same as -e '(require (planet \"<nm>.ss\" (\"<u>\" \"<nm>.plt\"))'\n"
	 "  -r, --script : Script mode: use as last option for scripts; same as -fmv-\n" 
	 "  -i, --script-cd : Like -r, but also sets the directory; same as -dmv-\n"
	 "  -u, --require-script : Like -r, but requires a module; same as -tmv-\n" 
# ifndef MZSCHEME_CMD_LINE
	 "  -Z, --nogui : Skip \"class.ss\" & \"mred.ss\" require, load handler set\n"
# endif
# ifdef CMDLINE_STDIO_FLAG
	 "  -z, --stdio : Show version, use read-eval-print-loop (not graphical-...)\n"
	 "  -K, --back : Don't bring application to the foreground (Mac OS X)\n"
# endif
	 "  -w, --awk : Same as -l awk.ss\n"
	 "  -k <n> <m> : Load executable-embedded code from file offset <n> to <m>\n"
	 "  -C, --main : Like -r, then call `main' w/argument list; car is file name\n"
	 " Initialization options:\n"
	 "  -X <dir>, --collects <dir> : Main collects at <dir> relative to " PROGRAM "\n"
	 "  -S <dir>, --search <dir> : More collects at <dir> relative to " PROGRAM "\n"
	 "  -U, --no-user-path : Ignores user-specific collects, etc.\n"
         "  -x, --no-lib-path : Skips trying to set current-library-collection-paths\n"
	 "  -q, --no-init-file : Skips trying to load " INIT_FILENAME "\n"
	 "  -N <file>, --name <file> : Sets `program' to <file>\n"
	 "  -A : Skips defining `argv' and `program'\n"
# ifdef MZ_USE_JIT
	 "  -j, --no-jit : Disables just-in-time compiler\n"
# else
	 "  -j, --no-jit : No effect, since the just-in-time compiler is unavailable\n"
# endif
	 " Language setting options:\n"
         "  -Q, --prim : Assume primitive bindings at top level\n"
	 "  -g, --case-sens : Identifiers/symbols are initially case-sensitive\n"
	 "  -G, --case-insens : Identifiers/symbols are initially case-insensitive\n"
	 "  -s, --set-undef : " SDESC "\n"
	 " Miscellaneous options:\n"
	 "  -- : No argument following this switch is used as a switch\n"
	 "  -m, --mute-banner : Suppresses "
# ifdef MZSCHEME_CMD_LINE  
	 "the startup banner"
# else
	 "-v/--version/-z/--stdio"
# endif
	 " text\n"
	 "  -v, --version : Suppresses the read-eval-print loop"
# ifdef MZSCHEME_CMD_LINE  
	 "\n"
# else
	 ", prints version\n"
# endif
# ifdef VERSION_YIELD_FLAG
	 "  -V, --no-yield : Like -v, also suppresses (yield 'wait)\n"
# endif
	 "  -b, --binary : Read stdin and write stdout/stderr in binary mode\n"
	 "  -h, --help : Shows this information and exits, ignoring other options\n"
	 "Multiple single-letter switches can be collapsed, with arguments placed\n"
	 " after the collapsed switches; the first collapsed switch cannot be --.\n"
	 " E.g.: `-vfme file expr' is the same as `-v -f file -m -e expr'\n"
	 "Extra arguments following the last option are put into the Scheme global\n"
	 " variable `argv' as a vector of strings. The name used to start " PROGRAM "\n"
	 " is put into the global variable `program' as a string.\n"
         "Expressions/files/--main/etc. are evaluated/loaded in order as provided.\n"
	 " An error during evaluation/loading causes later ones to be skipped.\n"
	 "The current-library-collection-paths is automatically set before any\n"
	 " expressions/files are evaluated/loaded, unless the -x or --no-lib-path\n"
	 " option is used.\n"
	 "The file " INIT_FILENAME
	 " is loaded before any provided expressions/files\n"
	 " are evaluated/loaded, unless the -q or --no-init-file option is used.\n"
# ifdef MACINTOSH_EVENTS
	 "\n"
	 "Macintosh Startup files are alphabetized and put after the -F switch\n"
	 " on the command line.\n"
	 "If a single startup file is provided and it begins with #!, it\n"
	 " is handled specially. Starting with the next whitespace, the rest\n"
	 " of the line is used as command line arguments. Unless #!! is used,\n"
	 " the startup file name is added to the end of this command line.\n"
# endif
	 "If the executable name has the form scheme-<dialect>, then the command\n"
	 " line is effectively prefixed with:\n"
	 "    -qAeC '(require (lib \"init.ss\" \"script-lang\" \"<dialect>\"))'\n"
	 " Thus, the first command-line argument serves as the name of a file;\n"
	 " the file should define `main', which is called with the arguments in a\n"
	 " list, starting with the path of the loaded file.\n"
	 "For general information about " PROGRAM ", see:\n"
	 "  http://www.plt-scheme.org/software/" PROGRAM_LC "/\n"
	 );
  PRINTF(prog, BANNER);
#if defined(WINDOWS_FILE_HANDLES)
  fflush(stdout);
#endif
  return 0;
 bad_switch:
  PRINTF("%s: bad switch %s\n", prog, real_switch);
 show_need_help:
  PRINTF("Use the --help or -h flag for help.\n");
#if defined(DETECT_WIN32_CONSOLE_STDIN)
  fflush(stdout);
#endif
  return 1;
#endif
}
