/*
 * File:        mred.c
 * Purpose:     GRacket main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1995-2000, Matthew Flatt
 */

#ifdef WIN32
/* Hack: overwrite "y" with "n" in binary to disable checking for another
   instance of the same app. */
char *check_for_another = "yes, please check for another";
static int wx_in_terminal = 0;
# define MZ_DEFINE_UTF8_MAIN
#endif

struct Scheme_Env;
static char *get_gr_init_filename(struct Scheme_Env *env);

#define UNIX_INIT_FILENAME "~/.gracketrc"
#define WINDOWS_INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\gracketrc.rktd"
#define MACOS9_INIT_FILENAME "PREFERENCES:gracketrc.rktd"
#define GET_INIT_FILENAME get_gr_init_filename
#if WIN32
# define NEED_CONSOLE_PRINTF
# define DEFER_EXPLICIT_EXIT
#else
# define PRINTF printf
#endif
#define PROGRAM "GRacket"
#define PROGRAM_LC "gracket"
#define INITIAL_BIN_TYPE "ri"

#define CMDLINE_STDIO_FLAG
#define YIELD_BEFORE_EXIT
#define INITIAL_NAMESPACE_MODULE "scheme/gui/init"
#define GRAPHICAL_REPL

static void yield_indefinitely();

# include "../racket/main.c"

static char *get_gr_init_filename(Scheme_Env *env)
{
  char *s, *s2;
  int len, i;

  s = get_init_filename(env);
  if (s) {
    len = strlen(s);
    for (i = len - 8; i; i--) {
      if (!strncmp(s XFORM_OK_PLUS i, "racketrc", 8)) {
        s2 = (char *)malloc(len + 2);
        memcpy(s2, s, i);
        memcpy(s2 + i + 1, s + i, len - i + 1);
        s2[i] = 'g';
        s = s2;
        break;
      }
    }
  }

  return s;
}

static void yield_indefinitely()
{
#ifdef MZ_PRECISE_GC
  void *dummy;
#endif
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  Scheme_Object *a[2], *yld;

  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    a[0] = scheme_intern_symbol("mred/mred");
    a[1] = scheme_intern_symbol("yield");
    yld = scheme_dynamic_require(2, a);

    a[0] = scheme_intern_symbol("wait");
    scheme_apply(yld, 1, a);
  }

  p->error_buf = save;

#ifdef MZ_PRECISE_GC
  dummy = NULL; /* makes xform think that dummy is live, so we get a __gc_var_stack__ */
#endif
}

#ifdef WIN32

static void MrEdSchemeMessages(char *, ...);
static Scheme_Object *stdin_pipe;

static HANDLE console_out;
static HANDLE console_in;
static Scheme_Object *console_inport;
static HWND console_hwnd;
static int has_stdio, stdio_kills_prog;
static HANDLE waiting_sema;

typedef HWND (WINAPI* gcw_proc)();

static void init_console_in()
{
  if (!console_in) {
    console_in = GetStdHandle(STD_INPUT_HANDLE);
    MZ_REGISTER_STATIC(console_inport);
    console_inport = scheme_make_fd_input_port((int)console_in, scheme_intern_symbol("stdin"), 0, 0);
  }
}

static BOOL WINAPI ConsoleHandler(DWORD op)
{
  if (stdio_kills_prog) {
    ReleaseSemaphore(waiting_sema, 1, NULL);
  } else {
    scheme_break_main_thread();
    scheme_signal_received();
  }
  return TRUE;
}

static void WaitOnConsole()
{
  DWORD wrote;

  stdio_kills_prog = 1;
  if (console_hwnd) {
    AppendMenu(GetSystemMenu(console_hwnd, FALSE), 
	       MF_STRING,
	       SC_CLOSE,
	       "Close");
    /* Un-gray the close box: */
    RedrawWindow(console_hwnd, NULL, NULL, 
		 RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW);
  }

  WriteConsole(console_out, "\n[Exited. Close box or Ctrl-C closes the console.]\n", 51, &wrote, NULL);

  WaitForSingleObject(waiting_sema, INFINITE);
}

static void MrEdSchemeMessages(char *msg, ...)
{
  GC_CAN_IGNORE va_list args;
  
  scheme_start_atomic();

  XFORM_HIDE_EXPR(va_start(args, msg));

  if (!console_out) {
    AllocConsole();
    console_out = GetStdHandle(STD_OUTPUT_HANDLE);

    if (!wx_in_terminal) {
      has_stdio = 1;
      waiting_sema = CreateSemaphore(NULL, 0, 1, NULL);
      SetConsoleCtrlHandler(ConsoleHandler, TRUE);      

      {
	HMODULE hm;
	gcw_proc gcw;

	hm = LoadLibrary("kernel32.dll");
	if (hm)
	  gcw = (gcw_proc)GetProcAddress(hm, "GetConsoleWindow");
	else
	  gcw = NULL;
    
	if (gcw)
	  console_hwnd = gcw();
      }

      if (console_hwnd) {
	EnableMenuItem(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE,
		       MF_BYCOMMAND | MF_GRAYED);
	RemoveMenu(GetSystemMenu(console_hwnd, FALSE), SC_CLOSE, MF_BYCOMMAND);
      }
    }
  }

  if (!msg) {
    char *s;
    long l, d;
    DWORD wrote;

    s = va_arg(args, char*);
    d = va_arg(args, long);
    l = va_arg(args, long);

    WriteConsole(console_out, s XFORM_OK_PLUS d, l, &wrote, NULL);
  } else {
    char *buffer;
    DWORD wrote;
    buffer = (char *)malloc(5 * strlen(msg));
    vsprintf(buffer, msg, args);
    WriteConsole(console_out, buffer, strlen(buffer), &wrote, NULL);
    free(buffer);
  }

  scheme_end_atomic_no_swap();

  XFORM_HIDE_EXPR(va_end(args));
}

static void MrEdSchemeMessagesOutput(char *s, long l)
{
  if (l)
    MrEdSchemeMessages(NULL, s, 0, l);
}

static Scheme_Object *console_reading;

static void add_console_reading()
{
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!console_reading) {
    MZ_REGISTER_STATIC(console_reading);
    console_reading = scheme_make_null();
  }

  console_reading = scheme_make_pair((Scheme_Object *)thread,
				     console_reading);
}

static void remove_console_reading()
{
  Scheme_Object *p, *prev = NULL;
  Scheme_Thread *thread;
  thread = scheme_get_current_thread();

  if (!console_reading)
    return;

  p = console_reading;
  while (SCHEME_PAIRP(p)) {
    if (SAME_OBJ(SCHEME_CAR(p), (Scheme_Object *)thread)) {
      if (prev)
	SCHEME_CDR(prev) = SCHEME_CDR(p);
      else
	console_reading = SCHEME_CDR(p);
      return;
    }
    prev = p;
    p = SCHEME_CDR(p);
  }
}

static void break_console_reading_threads()
{
  Scheme_Object *p;

  if (!console_reading)
    return;

  for (p = console_reading; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
    scheme_break_thread((Scheme_Thread *)SCHEME_CAR(p));
  }
}

static long mrconsole_get_string(Scheme_Input_Port *ip,
				 char *buffer, long offset, long size,
				 int nonblock, Scheme_Object *unless)
{
  long result;
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  add_console_reading();
  result = scheme_get_byte_string_unless("console get-string", pipe, 
					 buffer, offset, size, 
					 nonblock, 0, NULL,
					 unless);
  remove_console_reading();
  return result;
}

static Scheme_Object *mrconsole_progress_evt(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_progress_evt(pipe);
}

static int mrconsole_peeked_read(Scheme_Input_Port *ip,
					    long amount,
					    Scheme_Object *unless,
					    Scheme_Object *target_ch)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_peeked_read(pipe, amount, unless, target_ch);
}

static int mrconsole_char_ready(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_char_ready(pipe);
}

static void mrconsole_close(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;

  init_console_in();
  pipe = console_inport;

  scheme_close_input_port(pipe);
}

static Scheme_Object *MrEdMakeStdIn(void)
{
  Scheme_Object *readp;
  Scheme_Input_Port *ip;

  MZ_REGISTER_STATIC(stdin_pipe);

  scheme_pipe(&readp, &stdin_pipe);

  ip = scheme_make_input_port(scheme_make_port_type("mred-console-input-port"),
			      readp,
			      scheme_intern_symbol("mred-console"),
			      mrconsole_get_string,
			      NULL,
			      mrconsole_progress_evt,
			      mrconsole_peeked_read,
			      mrconsole_char_ready,
			      mrconsole_close,
			      NULL,
			      0);

  return (Scheme_Object *)ip;
}

static long stdout_write(Scheme_Output_Port*op, const char *s, long d, long l, 
			 int rarely_block, int enable_break)
{
  if (l)
    MrEdSchemeMessages(NULL, s, d, l);
  return l;
}

static Scheme_Object *MrEdMakeStdOut(void)
{
  Scheme_Object *outtype;

  outtype = scheme_make_port_type("stdout");

  return (Scheme_Object *)scheme_make_output_port(outtype, NULL,
						  scheme_intern_symbol("mred-console"),
						  scheme_write_evt_via_write,
						  stdout_write,
						  NULL, NULL, NULL, NULL, NULL, 0);
}

static long stderr_write(Scheme_Output_Port*op, const char *s, long d, long l, 
			 int rarely_block, int enable_break)
{
  if (l)
    MrEdSchemeMessages(NULL, s, d, l);
  return l;
}

static Scheme_Object *MrEdMakeStdErr(void)
{
  Scheme_Object *errtype;

  errtype = scheme_make_port_type("stderr");

  return (Scheme_Object *)scheme_make_output_port(errtype, NULL,
						  scheme_intern_symbol("mred-console"),
						  scheme_write_evt_via_write,
						  stderr_write,
						  NULL, NULL, NULL, NULL, NULL, 0);
}

static void MrEdExit(int v)
{
  if (has_stdio) {
    WaitOnConsole();
  }

  scheme_immediate_exit(v);
}

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

char *wchar_to_char(wchar_t *wa, int len)
{
  char *a;
  int l;

  l = scheme_utf8_encode((unsigned int *)wa, 0, len, 
			 NULL, 0,
			 1 /* UTF-16 */);
  a = (char *)malloc(l + 1);
  scheme_utf8_encode((unsigned int *)wa, 0, len, 
		     (unsigned char *)a, 0,
		     1 /* UTF-16 */);
  a[l] = 0;

  return a;
}

static int parse_command_line(char ***_command, char *buf)
{
  GC_CAN_IGNORE unsigned char *parse, *created, *write;
  int maxargs;
  int findquote = 0;
  char **command;
  int count = 0;

  maxargs = 49;
  command = (char **)malloc((maxargs + 1) * sizeof(char *));
  
  parse = created = write = (unsigned char *)buf;
  while (*parse) {
    while (*parse && isspace(*parse)) { parse++; }
    while (*parse && (!isspace(*parse) || findquote))	{
      if (*parse== '"') {
	findquote = !findquote;
      } else if (*parse== '\\') {
	GC_CAN_IGNORE unsigned char *next;
	for (next = parse; *next == '\\'; next++) { }
	if (*next == '"') {
	  /* Special handling: */
	  int count = (next - parse), i;
	  for (i = 1; i < count; i += 2) {
	    *(write++) = '\\';
	  }
	  parse += (count - 1);
	  if (count & 0x1) {
	    *(write++) = '\"';
	    parse++;
	  }
	}	else
	  *(write++) = *parse;
      } else
	*(write++) = *parse;
      parse++;
    }
    if (*parse)
      parse++;
    *(write++) = 0;
    
    if (*created)	{
      command[count++] = (char *)created;
      if (count == maxargs) {
	char **c2;
	c2 = (char **)malloc(((2 * maxargs) + 1) * sizeof(char *));
	memcpy(c2, command, maxargs * sizeof(char *));
	maxargs *= 2;
      }
    }
    created = write;
  }

  command[count] = NULL;
  *_command = command;

  return count;
}

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  LPWSTR m_lpCmdLine;
  int j, argc, l;
  char *a, **argv, *normalized_path;

  /* Order matters: load dependencies first */
# ifndef MZ_PRECISE_GC
  load_delayed_dll(NULL, "libmzgcxxxxxxx.dll");
# endif
  load_delayed_dll(NULL, "libracket" DLL_3M_SUFFIX "xxxxxxx.dll");
  record_dll_path();

  {
    HANDLE h;
    h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h && (h != INVALID_HANDLE_VALUE)
        && (GetFileType(h) != FILE_TYPE_UNKNOWN)) {
      wx_in_terminal = 1;
    }
  }

  /* Get command line: */
  m_lpCmdLine = GetCommandLineW();
  for (j = 0; m_lpCmdLine[j]; j++) {
  }
  a = wchar_to_char(m_lpCmdLine, j);

  argc = parse_command_line(&argv, a);

  /* argv[0] should be the name of the executable, but Windows doesn't
     specify really where this name comes from, so we get it from
     GetModuleFileName, just in case */
  {
    int name_len = 1024;
    while (1) {
      wchar_t *my_name;
      my_name = (wchar_t *)malloc(sizeof(wchar_t) * name_len);
      l = GetModuleFileNameW(NULL, my_name, name_len);
      if (!l) {
	name_len = GetLastError();
	free(my_name);
	my_name = NULL;
	break;
      } else if (l < name_len) {
	a = wchar_to_char(my_name, l);
	argv[0] = a;
	{
	  /* CharLowerBuff doesn't work with unicows.dll -- strange. 
	     So we use CharLower, instead. */
	  int i;
	  for (i = 0; i < l; i++) {
	    CharLowerW(my_name XFORM_OK_PLUS i);
	  }
	}
	normalized_path = wchar_to_char(my_name, l);
	free(my_name);
	break;
      } else {
	free(my_name);
	name_len = name_len * 2;
      }
    }
  }

  if (!wx_in_terminal) {
    scheme_set_stdio_makers(MrEdMakeStdIn,
			    MrEdMakeStdOut,
			    MrEdMakeStdErr);
    scheme_set_console_output(MrEdSchemeMessagesOutput);
  }
  scheme_set_console_printf(MrEdSchemeMessages);
  scheme_set_exit(MrEdExit);

  j = main(argc, argv);

  MrEdExit(j);
  /* shouldn't get here */

  return j;
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

#endif
