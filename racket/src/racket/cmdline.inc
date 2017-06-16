/****************************************************************/
/* This command-line parser is used by both Racket and GRacket. */
/****************************************************************/

#pragma GCC diagnostic ignored "-Wwrite-strings"

#define SDESC "Set! works on undefined identifiers"

char * volatile scheme_cmdline_exe_hack = (char *)
  ("[Replace me for EXE hack                                       "
   "                                                              ]");

#ifdef MZ_PRECISE_GC
# define GC_PRECISION_TYPE "3"
#else
# define GC_PRECISION_TYPE "c"
#endif
char * volatile scheme_binary_type_hack = "bINARy tYPe:" INITIAL_BIN_TYPE GC_PRECISION_TYPE;
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

char * volatile scheme_coldir = "coLLECTs dIRECTORy:" /* <- this tag stays, so we can find it again */
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

#ifndef INITIAL_CONFIG_DIRECTORY
# ifdef DOS_FILE_SYSTEM
#  define INITIAL_CONFIG_DIRECTORY "etc"
# else
#  define INITIAL_CONFIG_DIRECTORY "../etc"
# endif
#endif

char * volatile scheme_configdir = "coNFIg dIRECTORy:" /* <- this tag stays, so we can find it again */
                       INITIAL_CONFIG_DIRECTORY
                       "\0"
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
static int _configdir_offset = 17; /* Skip permanent tag */


#ifndef MZ_PRECISE_GC
# define XFORM_OK_PLUS +
#endif

#ifdef OS_X
# include <mach-o/getsect.h>
# include <mach-o/dyld.h>
# include <fcntl.h>
#endif

#ifdef DOS_FILE_SYSTEM
# include <windows.h>

#ifndef DLL_RELATIVE_PATH
# define DLL_RELATIVE_PATH L"lib"
#endif
#include "delayed.inc"

#ifdef NEED_CONSOLE_PRINTF
static void (*console_printf)(char *str, ...);
# define PRINTF console_printf
#endif

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

#ifdef OS_X
static long get_segment_offset()
{
# if defined(__x86_64__) || defined(__arm64__)
  const struct segment_command_64 *seg;
# else
  const struct segment_command *seg;
#endif
  seg = getsegbyname("__PLTSCHEME");
  if (seg)
    return seg->fileoff;
  else
    return 0;
}
#endif

#ifdef DOS_FILE_SYSTEM
wchar_t *get_self_executable_path()
{
  wchar_t *path;
  DWORD r, sz = 1024;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      free(path);
      sz = 2 * sz;
    } else
      break;
  }

  return path;
}

static DWORD find_by_id(HANDLE fd, DWORD rsrcs, DWORD pos, int id)
{
  DWORD got, val;
  WORD name_count, id_count;
  
  SetFilePointer(fd, pos + 12, 0, FILE_BEGIN);
  ReadFile(fd, &name_count, 2, &got, NULL);
  ReadFile(fd, &id_count, 2, &got, NULL);

  pos += 16 + (name_count * 8);
  while (id_count--) {
    ReadFile(fd, &val, 4, &got, NULL);
    if (val == id) {
      ReadFile(fd, &val, 4, &got, NULL);
      return rsrcs + (val & 0x7FFFFFF);
    } else {
      ReadFile(fd, &val, 4, &got, NULL);
    }
  }

  return 0;
}

static long get_segment_offset()
{
  /* Find the resource of type 257 */
  wchar_t *path;
  HANDLE fd;

  path = get_self_executable_path();
  fd = CreateFileW(path, GENERIC_READ,
                   FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL,
                   OPEN_EXISTING,
                   0,
                   NULL);
  free(path);

  if (fd == INVALID_HANDLE_VALUE)
    return 0;
  else {
    DWORD val, got, sec_pos, virtual_addr, rsrcs, pos;
    WORD num_sections, head_size;
    char name[8];
    
    SetFilePointer(fd, 60, 0, FILE_BEGIN);
    ReadFile(fd, &val, 4, &got, NULL);
    SetFilePointer(fd, val+4+2, 0, FILE_BEGIN); /* Skip "PE\0\0" tag and machine */
    ReadFile(fd, &num_sections, 2, &got, NULL);
    SetFilePointer(fd, 12, 0, FILE_CURRENT); /* time stamp + symbol table */
    ReadFile(fd, &head_size, 2, &got, NULL);

    sec_pos = val+4+20+head_size;
    while (num_sections--) {
      SetFilePointer(fd, sec_pos, 0, FILE_BEGIN);
      ReadFile(fd, &name, 8, &got, NULL);
      if ((name[0] == '.')
          && (name[1] == 'r')
          && (name[2] == 's')
          && (name[3] == 'r')
          && (name[4] == 'c')
          && (name[5] == 0)) {
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip virtual size */
        ReadFile(fd, &virtual_addr, 4, &got, NULL);
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip file size */
        ReadFile(fd, &rsrcs, 4, &got, NULL);
        SetFilePointer(fd, rsrcs, 0, FILE_BEGIN);
        
        /* We're at the resource table; step through 3 layers */
        pos = find_by_id(fd, rsrcs, rsrcs, 257);
	if (pos) {
	  pos = find_by_id(fd, rsrcs, pos, 1);
	  if (pos) {
	    pos = find_by_id(fd, rsrcs, pos, 1033);

	    if (pos) {
	      /* pos is the reource data entry */
	      SetFilePointer(fd, pos, 0, FILE_BEGIN);
	      ReadFile(fd, &val, 4, &got, NULL);
	      pos = val - virtual_addr + rsrcs;

	      CloseHandle(fd);

	      return pos;
	    }
	  }
	}

	break;
      }
      sec_pos += 40;
    }

    /* something went wrong */
    CloseHandle(fd);
    return 0;
  }
}
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

#if defined(OS_X) || defined(DOS_FILE_SYSTEM)
char *add_to_str(const char *addr, long amt)
{
  long addr_v;
  char buf[32];
  addr_v = atoi(addr) + amt;
  sprintf(buf, "%ld", addr_v);
  return strdup(buf);
}
#endif

static char *make_embedded_load(const char *start, const char *end)
{
  char *s;
  int slen, elen;

#if defined(OS_X) || defined(DOS_FILE_SYSTEM)
  {
    long fileoff;
    fileoff = get_segment_offset();
    start = add_to_str(start, fileoff);
    end = add_to_str(end, fileoff);
  }
#endif

  slen = strlen(start);
  elen = strlen(end);

  s = (char *)malloc(slen + elen + 2);
  memcpy(s, start, slen + 1);
  memcpy(s + slen + 1, end, elen + 1);

  return s;
}

static Scheme_Object *check_make_path(const char *prog, const char *real_switch, char *arg)
{
  if (!*arg) {
    PRINTF("%s: empty path after %s switch\n",
           prog, real_switch);
    exit(1);
  }
  return scheme_make_path(arg);
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
  mzcmd_EMBEDDED_REG    = 7,
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
#ifndef NO_YIELD_BEFORE_EXIT
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

typedef void (*Repl_Proc)(Scheme_Env *, FinishArgs *f);

static void configure_environment(Scheme_Object *mod)
{
  Scheme_Object *mli, *dyreq, *a[3], *gi, *v, *vs;
  Scheme_Object *submod, *cr, *mdp, *mpij;

  cr = scheme_intern_symbol("configure-runtime");

  /* Modern style: look for `runtime-configure' submodule to initialize
     the configuration: */

  submod = scheme_intern_symbol("submod");
  cr = scheme_intern_symbol("configure-runtime");

  if (SAME_TYPE(SCHEME_TYPE(mod), scheme_module_index_type)) {
    mpij = scheme_builtin_value("module-path-index-join");
    a[0] = scheme_make_pair(submod, 
                            scheme_make_pair(scheme_make_utf8_string("."),
                                             scheme_make_pair(cr, scheme_make_null())));
    a[1] = mod;
    submod = scheme_apply(mpij, 2, a);
  } else if (SCHEME_PAIRP(mod) && SAME_OBJ(SCHEME_CAR(mod), submod))
    submod = scheme_append(mod, scheme_make_pair(cr, scheme_make_null()));
  else
    submod = scheme_make_pair(submod, 
                              scheme_make_pair(mod,
                                               scheme_make_pair(cr, scheme_make_null())));
  mdp = scheme_builtin_value("module-declared?");

  a[0] = submod;
  a[1] = scheme_make_true();
  v = scheme_apply(mdp, 2, a);
  if (!SAME_OBJ(scheme_make_false(), v)) {
    dyreq = scheme_builtin_value("dynamic-require");

    a[0] = submod;
    a[1] = scheme_make_false();
    (void)scheme_apply(dyreq, 2, a);
  }

  /* Old style: use `module->language-info' (after new style, for 
     compatibility): */

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

    a[0] = cr;
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
                 || (fa->eval_kind[i] == mzcmd_EMBEDDED)
                 || (fa->eval_kind[i] == mzcmd_EMBEDDED_REG)) {
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
          } else if (fa->eval_kind[i] == mzcmd_EMBEDDED_REG) {
            scheme_register_embedded_load(-1, fa->evals_and_loads[i]);
            scheme_embedded_load(-1, fa->evals_and_loads[i], 1);
          } else if (fa->eval_kind[i] == mzcmd_EMBEDDED) {
            scheme_embedded_load(-1, fa->evals_and_loads[i], 0);
          } else {
            Scheme_Object *a[2], *nsreq, *mpi;
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
            if (!scheme_is_module_path(a[0])) {
              scheme_signal_error("require: bad module path: %V derived from command-line argument: %s",
                                  a[0],
                                  fa->evals_and_loads[i]);
            }
            /* Use a module path index so that multiple resolutions are no unduly
               sensitive to changes in the current directory or other configurations: */
            mpi = scheme_make_modidx(a[0], scheme_make_false(), scheme_make_false());
            if (!did_config)
              configure_environment(mpi);
            /* Run the module: */
            a[0] = mpi;
            scheme_apply(nsreq, 1, a);
            /* If there's a `main' submodule, run it: */
            a[0] = scheme_make_modidx(scheme_make_pair(scheme_intern_symbol("submod"),
                                                       scheme_make_pair(scheme_make_utf8_string("."),
                                                                        scheme_make_pair(scheme_intern_symbol("main"),
                                                                                         scheme_make_null()))),
                                      mpi,
                                      scheme_make_false());
            if (scheme_module_is_declared(a[0], 1)) {
              a[1] = scheme_make_false();
              scheme_apply(scheme_builtin_value("dynamic-require"), 2, a);
            }
          }
        } else {
          scheme_clear_escape();
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
          scheme_clear_escape();
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
      scheme_clear_escape();
      exit_val = 1;
    }
    p->error_buf = save;
  }
#endif

#ifndef DONT_LOAD_INIT_FILE
  if (fa->a->use_repl && !fa->a->no_init_file) {
    Scheme_Object *a[2];
    a[0] = get_init_filename(fa->global_env,
                             INIT_FILENAME_CONF_SYM,
                             DEFAULT_INIT_MODULE,
                             USER_INIT_MODULE);
    if (a[0]) {
        mz_jmp_buf * volatile save, newbuf;
        Scheme_Thread * volatile p;
        p = scheme_get_current_thread();
        save = p->error_buf;
        p->error_buf = &newbuf;
        if (!scheme_setjmp(newbuf)) {
          a[1] = scheme_make_integer(0);
          scheme_dynamic_require(2, a);
        } else {
          scheme_clear_escape();
          exit_val = 1;
        }
        p->error_buf = save;
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
      repl(fa->global_env, fa);
      exit_val = 0;
    } else {
      scheme_clear_escape();
      exit_val = 1;
#ifndef NO_YIELD_BEFORE_EXIT
      fa->a->add_yield = 0;
#endif
    }
    p->error_buf = save;
  }
#endif /* DONT_RUN_REP */

#ifndef NO_YIELD_BEFORE_EXIT
  if (fa->a->add_yield) {
    mz_jmp_buf * volatile save, newbuf;
    Scheme_Thread * volatile p;
    Scheme_Object *yh, *yha[1];
    p = scheme_get_current_thread();
    save = p->error_buf;
    p->error_buf = &newbuf;
    if (!scheme_setjmp(newbuf)) {
      yh = scheme_get_param(scheme_current_config(), MZCONFIG_EXE_YIELD_HANDLER);
      yha[0] = scheme_make_integer(exit_val);
      scheme_apply(yh, 1, yha);
    } else {
      scheme_clear_escape();
    }
    p->error_buf = save;
  }
#endif

  if (scheme_flush_managed(NULL, 1))
    exit_val = 1;

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

#ifndef MZ_XFORM
# define GC_CAN_IGNORE /**/
#endif

#include <ctype.h>

static Scheme_Object *get_log_level(char *prog, char *real_switch, const char *envvar, const char *what, GC_CAN_IGNORE char *str)
{
  int k, len, default_lvl = -1;
  Scheme_Object *l, *last = NULL;
  GC_CAN_IGNORE char *s, *orig_str = str;

  l =  scheme_make_null();

  while (1) {
    while (*str && isspace(*((unsigned char *)str))) {
      str++;
    }

    if (!*str) {
      if (default_lvl == -1) default_lvl = 0;
      if (last)
        SCHEME_CDR(last) = scheme_make_integer(default_lvl);
      else
        l = scheme_make_integer(default_lvl);
      return l;
    }

    if (!strncmp(str, "none", 4)) {
      k = 0;
      len = 4;
    } else if (!strncmp(str, "fatal", 5)) {
      k = SCHEME_LOG_FATAL;
      len = 5;
    } else if (!strncmp(str, "error", 5)) {
      k = SCHEME_LOG_ERROR;
      len = 5;
    } else if (!strncmp(str, "warning", 7)) {
      k = SCHEME_LOG_WARNING;
      len = 7;
    } else if (!strncmp(str, "info", 4)) {
      k = SCHEME_LOG_INFO;
      len = 4;
    } else if (!strncmp(str, "debug", 5)) {
      k  = SCHEME_LOG_DEBUG;
      len = 5;
    } else {
      k = -1;
      len = 0;
    }

    str += len;

    if (k != -1) {
      if (*str == '@') {
        str++;
        for (s = str; *s && !isspace(*((unsigned char *)s)); s++) {
        }
        l = scheme_make_pair(scheme_make_sized_byte_string(str, s - str, 1), l);
        if (!last) last = l;
        l = scheme_make_pair(scheme_make_integer(k), l);
        str = s;
      } else if (isspace(*((unsigned char *)str)) || !*str) {
        if (default_lvl == -1)
          default_lvl = k;
        else
          k = -1;
      } else
        k = -1;
      if (*str) str++;
    }

    if (k == -1) {
      PRINTF("%s: %s <levels> %s%s%s must be one of the following\n"
             " <level>s:\n"
             "   none fatal error warning info debug\n"
             " or up to one such <level> in whitespace-separated sequence of\n"
             "   <level>@<name>\n"
             " given: %s\n",
             prog, what, 
             real_switch ? "after " : "in ",
             real_switch ? real_switch : envvar,
             real_switch ? " switch" : " environment variable",
             orig_str);
      return NULL;
    }
  }
}

static Scheme_Object *get_arg_log_level(char *prog, char *real_switch, const char *what, int argc, char **argv)
{
  if (argc < 2) {
    PRINTF("%s: missing %s level after %s switch\n", 
           prog, what, real_switch);
    return NULL;
  }

  return get_log_level(prog, real_switch, NULL, what, argv[1]);
}

static Scheme_Object *adjust_collects_path(Scheme_Object *collects_path, int *_skip_coll_dirs)
{
  /* Setup path for "collects" collection directory: */
  if (!collects_path) {
    if (!scheme_coldir[_coldir_offset])
      collects_path = scheme_make_false();
    else
      collects_path = scheme_make_path(scheme_coldir XFORM_OK_PLUS _coldir_offset);
  } else if (!SAME_OBJ(collects_path, scheme_make_false()))
    collects_path = scheme_path_to_complete_path(collects_path, NULL);

  if (SAME_OBJ(collects_path, scheme_make_false())) {
    /* empty list of directories => don't set collection dirs
       and don't use collection links files */
    if (_skip_coll_dirs) {
      *_skip_coll_dirs = 1;
      scheme_set_ignore_link_paths(1);
    }
    collects_path = scheme_make_path(".");
  }

  return collects_path;
}

static Scheme_Object *adjust_config_path(Scheme_Object *config_path)
{
  if (!config_path) {
    char *s;
    s = getenv("PLTCONFIGDIR");
    if (s) {
      s = scheme_expand_filename(s, -1, NULL, NULL, 0);
      if (s) config_path = scheme_make_path(s);
    }
  }

  if (!config_path)
    config_path = scheme_make_path(scheme_configdir XFORM_OK_PLUS _configdir_offset);
  else
    config_path = scheme_path_to_complete_path(config_path, NULL);

  return config_path;
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

static Scheme_Object *create_cmdline_args_vector(int argc, char *args[])
{
  int i;
  Scheme_Object *v;
  v = scheme_make_vector(argc, NULL);
  for (i = 0; i < argc; i++) {
    Scheme_Object *so;
    so = scheme_make_locale_string(args[i]);
    SCHEME_SET_CHAR_STRING_IMMUTABLE(so);
    SCHEME_VEC_ELS(v)[i] = so;
  }
  if (argc)
    SCHEME_SET_VECTOR_IMMUTABLE(v);
  return v;
}


static int run_from_cmd_line(int argc, char *_argv[], 
			     Scheme_Env *(*mk_basic_env)(void),
			     int (*cont_run)(FinishArgs *f))
{
  GC_CAN_IGNORE char **argv = _argv;
  Scheme_Env *global_env;
  char *prog, *sprog = NULL;
  Scheme_Object *sch_argv;
  int skip_coll_dirs = 0;
  Scheme_Object *collects_path = NULL, *collects_extra = NULL, *addon_dir = NULL;
  Scheme_Object *config_path = NULL;
  Scheme_Object *host_collects_path = NULL, *host_config_path = NULL;
  char *compiled_paths = NULL;
  Scheme_Object *collects_paths_l, *collects_paths_r;
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
#ifndef NO_YIELD_BEFORE_EXIT
  int add_yield = 1;
#endif
#ifdef CMDLINE_STDIO_FLAG
  int alternate_rep = 0;
  int no_front = 0;
  char *wm_class = NULL;
#endif
  char *init_lib = INITIAL_NAMESPACE_MODULE;
  int was_config_flag = 0, saw_nc_flag = 0;
  int no_compiled = 0;
  int init_ns = 0, no_init_ns = 0;
  int cross_compile = 0;
  Scheme_Object *syslog_level = NULL, *stderr_level = NULL;
  FinishArgs *fa;
  FinishArgsAtoms *fa_a;

#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_PROCESS_ATTACH, NULL);
#endif

  prog = argv[0];
  argv++;
  --argc;

#ifdef NEED_CONSOLE_PRINTF
  console_printf = scheme_get_console_printf();
#endif
  
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

  /* If scheme_cmdline_exe_hack is changed, then we extract built-in
     arguments. */
  if (scheme_cmdline_exe_hack[0] != '[') {
    int n, i;
    long d;
    GC_CAN_IGNORE unsigned char *p;
    GC_CAN_IGNORE unsigned char *orig_p;
    char **argv2;

    p = NULL;
#ifdef DOS_FILE_SYSTEM
    if ((scheme_cmdline_exe_hack[0] == '?')
	|| (scheme_cmdline_exe_hack[0] == '*')) {
      /* This is how we make launchers in Windows. The cmdline is
	 added as a resource of type 257. The long integer at
	 scheme_cmdline_exe_hack[4] says where the command line starts
	 with the source, and scheme_cmdline_exe_hack[8] says how long
	 the cmdline string is. It might be relative to the
	 executable. */
      HANDLE fd;
      wchar_t *path;

      path = get_self_executable_path();
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
	start = *(long *)&scheme_cmdline_exe_hack[4];
	len = *(long *)&scheme_cmdline_exe_hack[8];
	start += get_segment_offset();
	p = (unsigned char *)malloc(len);
	SetFilePointer(fd, start, 0, FILE_BEGIN);
	ReadFile(fd, p, len, &got, NULL);
	CloseHandle(fd);
	if (got != len)
	  p = (unsigned char *)"\0\0\0";
	else if (scheme_cmdline_exe_hack[0] == '*') {
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
      free(path);
    }
#endif
#if defined(OS_X)
    if (scheme_cmdline_exe_hack[0] == '?') {
      long fileoff, cmdoff, cmdlen;
      int fd;
      fileoff = get_segment_offset();

      p = (unsigned char *)scheme_cmdline_exe_hack + 4;
      cmdoff = (p[0]
                + (((long)p[1]) << 8)
                + (((long)p[2]) << 16)
                + (((long)p[3]) << 24));
      cmdlen = (p[4]
                + (((long)p[5]) << 8)
                + (((long)p[6]) << 16)
                + (((long)p[7]) << 24));
      p = malloc(cmdlen);
      
      fd = open(_dyld_get_image_name(0), O_RDONLY);
      lseek(fd, fileoff + cmdoff, 0);
      read(fd, p, cmdlen);
      close(fd);
    }
#endif
     
    if (!p)
      p = (unsigned char *)scheme_cmdline_exe_hack + 1;

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
    else if (!strcmp("--no-user-path", argv[0]))
      argv[0] = "-U";
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
    else if (!strcmp("--compiled", argv[0]))
      argv[0] = "-R";
    else if (!strcmp("--addon", argv[0]))
      argv[0] = "-A";
    else if (!strcmp("--config", argv[0]))
      argv[0] = "-G";
    else if (!strcmp("--cross", argv[0]))
      argv[0] = "-C";
# ifdef CMDLINE_STDIO_FLAG
    else if (!strcmp("--stdio", argv[0]))
      argv[0] = "-z";
    else if (!strcmp("--back", argv[0]))
      argv[0] = "-K";
    else if (!strcmp("--wm-class", argv[0]))
      argv[0] = "-J";
# endif
# ifndef NO_YIELD_BEFORE_EXIT
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
          if (!*(argv[0])) {
            /* #f => no collects path */
            collects_path = scheme_make_false();
          } else
            collects_path = check_make_path(prog, real_switch, argv[0]);
          was_config_flag = 1;
	  break;
	case 'G':
	  if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
          config_path = check_make_path(prog, real_switch, argv[0]);
          was_config_flag = 1;
	  break;
        case 'R':
          if (argc < 2) {
	    PRINTF("%s: missing path after %s switch\n", 
		   prog, real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  compiled_paths = argv[0];
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
	  addon_dir = check_make_path(prog, real_switch, argv[0]);
          was_config_flag = 1;
            break;
        case 'C':
          if (!cross_compile) {
            cross_compile = 1;
            scheme_set_cross_compile_mode(1);
            was_config_flag = 1;
            host_config_path = config_path;
            host_collects_path = collects_path;
          }
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
          collects_extra = scheme_make_pair(check_make_path(prog, real_switch, argv[0]), collects_extra);
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
	  if (argc < 4) {
	    PRINTF("%s: missing %s after %s switch\n", 
		   prog, 
		   (argc < 2) ? "starting and ending offsets" : "second ending offset",
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
	  se = make_embedded_load(argv[0], argv[1]);
	  evals_and_loads[num_enl] = se;
	  argv++;
	  --argc;
	  eval_kind[num_enl++] = mzcmd_EMBEDDED_REG;
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
          if (!*sprog) {
            PRINTF("%s: empty path after %s switch\n", 
		   prog, 
		   real_switch);
	    goto show_need_help;
          }
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
#ifndef NO_YIELD_BEFORE_EXIT
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
          was_config_flag = 1;
	  break;
        case 'J':
          if (argc < 2) {
	    PRINTF("%s: missing name after %s switch\n", 
		   prog, 
		   real_switch);
	    goto show_need_help;
	  }
	  argv++;
	  --argc;
          wm_class = argv[0];
          was_config_flag = 1;
          break;
#endif
#ifdef USE_OSKIT_CONSOLE
	case 'S':
	  osk_not_console = 1;
	  break;
#endif
        case 'W':
          stderr_level = get_arg_log_level(prog, real_switch, "stderr", argc, argv);
          if (!stderr_level)
            goto show_need_help;
          --argc;
          argv++;
          was_config_flag = 1;
          break;
	case 'L':
          syslog_level = get_arg_log_level(prog, real_switch, "syslog", argc, argv);
          if (!syslog_level)
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

   CMDLINE_FFLUSH(stdout);
  }
#endif /* DONT_PARSE_COMMAND_LINE */

  if (!syslog_level) {
    char *s;
    s = getenv("PLTSYSLOG");
    if (s) {
      syslog_level = get_log_level(prog, NULL, "PLTSYSLOG", "syslog", s);
    }
  }
  if (!stderr_level) {
    char *s;
    s = getenv("PLTSTDERR");
    if (s) {
      stderr_level = get_log_level(prog, NULL, "PLTSTDERR", "stderr", s);
    }
  }
  if (getenv("PLTDISABLEGC")) {
    scheme_enable_garbage_collection(0);
  }
  {
    char *s;
    s = getenv("PLT_INCREMENTAL_GC");
    if (s) {
      if ((s[0] == '0') || (s[0] == 'n') || (s[0] == 'N'))
        scheme_incremental_garbage_collection(0);
      else if ((s[0] == '1') || (s[0] == 'y') || (s[0] == 'Y'))
        scheme_incremental_garbage_collection(1);
      else {
        PRINTF("%s: unrecognized value for PLT_INCREMENTAL_GC;\n"
                " a value that starts \"1\", \"y\", or \"Y\" permanently enables incremental mode,\n"
                " and a value that starts \"0\", \"n\", or \"N\" disables incremental mode,\n"
                " and the default enables incremental mode as requested via `collect-garbage'\n"
                "  unrecognized value: %s\n",
                prog, s);
      }
    }
  }
  {
    char *s;
    s = getenv("PLT_COMPILED_FILE_CHECK");
    if (s) {
      if (!strcmp(s, "modify-seconds"))
        scheme_set_compiled_file_check(SCHEME_COMPILED_FILE_CHECK_MODIFY_SECONDS);
      else if (!strcmp(s, "exists"))
        scheme_set_compiled_file_check(SCHEME_COMPILED_FILE_CHECK_EXISTS);
      else {
        PRINTF("%s: unrecognized value for PLT_COMPILED_FILE_CHECK;\n"
                " recognized values are \"modify-seconds\" and \"exists\"\n"
                "  unrecognized value: %s\n",
                prog, s);
      }
    }
  }

  scheme_set_logging_spec(syslog_level, stderr_level);

  collects_path = adjust_collects_path(collects_path, &skip_coll_dirs);
  scheme_set_collects_path(collects_path);

  if (cross_compile) {
    host_collects_path = adjust_collects_path(host_collects_path, NULL);
    scheme_set_host_collects_path(host_collects_path);
  }

  config_path = adjust_config_path(config_path);
  scheme_set_config_path(config_path);

  if (cross_compile) {
    host_config_path = adjust_config_path(host_config_path);
    scheme_set_host_config_path(host_config_path);
  }

  /* Make list of additional collection paths: */
  if (collects_extra)
    collects_paths_r = reverse_path_list(collects_extra, 1);
  else
    collects_paths_r = scheme_make_null();
  
  {
    int len, offset;
    collects_paths_l = scheme_make_null();
    offset = _coldir_offset;
    while (1) {
      len = strlen(scheme_coldir XFORM_OK_PLUS offset);
      offset += len + 1;
      if (!scheme_coldir[offset])
        break;
      collects_paths_l = scheme_make_pair(scheme_make_path(scheme_coldir XFORM_OK_PLUS offset), 
                                          collects_paths_l);
    }
    collects_paths_l = reverse_path_list(collects_paths_l, 0);
  }

  sch_argv = create_cmdline_args_vector(argc, argv);
  scheme_set_command_line_arguments(sch_argv);
  
  scheme_set_exec_cmd(prog);
  if (!sprog)
    sprog = prog;
  
  (void)scheme_set_run_cmd(sprog);
#ifdef CAN_SET_OS_PROCESS_NAME
  set_os_process_name(sprog);
#endif
  
  if (no_compiled)
    scheme_set_compiled_file_paths(scheme_make_null());

  /* Setup compiled-file search path: */
  if (!compiled_paths) {
    compiled_paths = getenv("PLTCOMPILEDROOTS");
  }

  /* Setup path for "addon" directory: */
  if (!addon_dir) {
    char *s;
    s = getenv("PLTADDONDIR");
    if (s) {
      s = scheme_expand_filename(s, -1, NULL, NULL, 0);
      if (s) addon_dir = scheme_make_path(s);
    }
  }
  if (addon_dir) {
    addon_dir = scheme_path_to_complete_path(addon_dir, NULL);
    scheme_set_addon_dir(addon_dir);
  }

  /* Creates the main kernel environment */
  global_env = mk_basic_env();

  if (!skip_coll_dirs)
    scheme_init_collection_paths_post(global_env, collects_paths_l, collects_paths_r);
  if (compiled_paths)
    scheme_init_compiled_roots(global_env, compiled_paths);

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
#ifndef NO_YIELD_BEFORE_EXIT
  fa->a->add_yield = add_yield;
#endif
#ifdef CMDLINE_STDIO_FLAG
  fa->a->alternate_rep = alternate_rep;
  fa->a->no_front = no_front;
  if (no_front)
    scheme_register_process_global("Racket-GUI-no-front", (void *)0x1);
  if (wm_class)
    scheme_register_process_global("Racket-GUI-wm-class", (void *)wm_class);
#endif
  fa->init_lib = init_lib;
  fa->global_env = global_env;

  scheme_set_can_break(1);

  return cont_run(fa);

#ifndef DONT_PARSE_COMMAND_LINE
 show_help:
  prog = (char *) ("%s"
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
	 "  -t <file>, --require <file> : Like -e '(require (file \"<file>\"))' [*]\n"
	 "  -l <path>, --lib <path> : Like -e '(require (lib \"<path>\"))' [*]\n"
	 "  -p <package> : Like -e '(require (planet \"<package>\")' [*]\n"
	 "  -r <file>, --script <file> : Same as -f <file> -N <file> --\n" 
	 "  -u <file>, --require-script <file> : Same as -t <file> -N <file> --\n" 
	 "  -k <n> <m> <p> : Load executable-embedded code from offset <n> to <p>\n"
	 "  -m, --main : Call `main' with command-line arguments, print results\n"
         "  [*] Also `require's a `main' submodule, if any\n"
	 " Interaction options:\n"
         "  -i, --repl : Run interactive read-eval-print loop; implies -v\n"
         "  -n, --no-lib : Skip `(require (lib \"<init-lib>\"))' for -i/-e/-f/-r\n"
	 "  -v, --version : Show version\n"
# ifdef CMDLINE_STDIO_FLAG
	 "  -K, --back : Don't bring application to the foreground (Mac OS X)\n"
# endif
# ifndef NO_YIELD_BEFORE_EXIT
	 "  -V, --no-yield : Skip `((executable-yield-handler) <status>)' on exit\n"
# endif
	 " Configuration options:\n"
         "  -c, --no-compiled : Disable loading of compiled files\n"
	 "  -q, --no-init-file : Skip load of " INIT_FILENAME " for -i\n"
# ifdef CMDLINE_STDIO_FLAG
	 "  -z, --text-repl : Use text `read-eval-print-loop' for -i\n"
# endif
         "  -I <path> : Set <init-lib> to <path> (sets language)\n"
	 "  -X <dir>, --collects <dir> : Main collects at <dir> (or \"\" disables all)\n"
	 "  -S <dir>, --search <dir> : More collects at <dir> (after main collects)\n"
	 "  -G <dir>, --config <dir> : Main configuration directory at <dir>\n"
	 "  -A <dir>, --addon <dir> : Addon directory at <dir>\n"
	 "  -U, --no-user-path : Ignore user-specific collects, etc.\n"
         "  -R <paths>, --compiled <paths> : Set compiled-file search roots to <paths>\n"
	 "  -C, --cross : Cross-build mode; save current collects and config as host\n"
	 "  -N <file>, --name <file> : Sets `(find-system-path 'run-file)' to <file>\n"
# ifdef CMDLINE_STDIO_FLAG
         "  -J <name>, ---wm-class <name> : Set WM_CLASS class to <name> (Unix)\n"
# endif
# ifdef MZ_USE_JIT
	 "  -j, --no-jit : Disable the just-in-time compiler\n"
# else
	 "  -j, --no-jit : No effect, since the just-in-time compiler is unavailable\n"
# endif
         "  -d, --no-delay: Disable on-demand loading of syntax and code\n"
	 "  -b, --binary : Read stdin and write stdout/stderr in binary mode\n"
	 "  -W <levels>, --warn <levels> : Set stderr logging to <levels>\n"
	 "  -L <levels>, --syslog <levels> : Set syslog logging to <levels>\n"
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
# ifndef NO_YIELD_BEFORE_EXIT
         " 6. Run `((executable-yield-handler) <status>)' [unless -V]\n"
# endif
	 );
  PRINTF(prog, BANNER);
#if defined(WINDOWS_FILE_HANDLES)
  CMDLINE_FFLUSH(stdout);
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
  CMDLINE_FFLUSH(stdout);
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
