/*
 * File:        mred.c
 * Purpose:     GRacket main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2014 PLT Design Inc.
 * Copyright:   (c) 1995-2000, Matthew Flatt
 */

#ifdef WIN32
/* Hack: overwrite "y" with "n" in binary to disable checking for another
   instance of the same app. */
char *check_for_another = "yes, please check for another";
static int wx_in_terminal = 0;
# define MZ_DEFINE_UTF8_MAIN
# define PRE_FILTER_CMDLINE_ARGUMENTS
static void pre_filter_cmdline_arguments(int *argc, char ***argv);
# define WINMAIN_ALREADY
# undef wx_xt
#endif

struct Scheme_Env;
static char *get_gr_init_filename(struct Scheme_Env *env);

#ifdef wx_xt
# define PRE_FILTER_CMDLINE_ARGUMENTS
static void pre_filter_cmdline_arguments(int *argc, char ***argv);
#endif

#ifdef wx_mac
# define PRE_FILTER_CMDLINE_ARGUMENTS
static void pre_filter_cmdline_arguments(int *argc, char ***argv);
#endif

#define UNIX_INIT_FILENAME "~/.gracketrc"
#define WINDOWS_INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\gracketrc.rktl"
#define MACOS9_INIT_FILENAME "PREFERENCES:gracketrc.rktl"
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
#define INITIAL_NAMESPACE_MODULE "racket/gui/init"
#define GRAPHICAL_REPL

#if WIN32
# define DLL_RELATIVE_PATH L"."
# ifndef INITIAL_COLLECTS_DIRECTORY
#  define INITIAL_COLLECTS_DIRECTORY "../collects"
# endif
#endif

#ifndef INITIAL_CONFIG_DIRECTORY
# define INITIAL_CONFIG_DIRECTORY "../etc"
#endif

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

/***********************************************************************/
/*                        Win32 handling                               */
/***********************************************************************/

#ifdef WIN32

/* ---------------------------------------- */
/*             stdio to console             */
/* ---------------------------------------- */

static void MrEdSchemeMessages(char *, ...);
static Scheme_Object *stdin_pipe;

static HANDLE console_out;
static HANDLE console_in;
static Scheme_Object *console_inport;
static HWND console_hwnd;
static int has_stdio, stdio_kills_prog;
static HANDLE waiting_sema;
static void *orig_signal_handle;
static void *orig_break_handle;

typedef HWND (WINAPI* gcw_proc)();

static void init_console_in()
{
  if (!console_in) {
    console_in = GetStdHandle(STD_INPUT_HANDLE);
    MZ_REGISTER_STATIC(console_inport);
    console_inport = scheme_make_fd_input_port((intptr_t)console_in, scheme_intern_symbol("stdin"), 0, 0);
  }
}

static BOOL WINAPI ConsoleHandler(DWORD op)
{
  if (stdio_kills_prog) {
    ReleaseSemaphore(waiting_sema, 1, NULL);
  } else {
    scheme_break_main_thread_at(orig_break_handle);
    scheme_signal_received_at(orig_signal_handle);
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
      orig_signal_handle = scheme_get_signal_handle();
      orig_break_handle = scheme_get_main_thread_break_handle();
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
    intptr_t l, d;
    DWORD wrote;

    s = va_arg(args, char*);
    d = va_arg(args, intptr_t);
    l = va_arg(args, intptr_t);

    WriteConsole(console_out, s XFORM_OK_PLUS d, l, &wrote, NULL);
  } else {
    char *buffer;
    DWORD wrote;
    /* FIXME: multiplying by 5 and adding 80 works for
       all the cases where printf mode is currently used 
       for the function, but it's completely a hack. */
    buffer = (char *)malloc((5 * strlen(msg)) + 80);
    vsprintf(buffer, msg, args);
    WriteConsole(console_out, buffer, strlen(buffer), &wrote, NULL);
    free(buffer);
  }

  scheme_end_atomic_no_swap();

  XFORM_HIDE_EXPR(va_end(args));
}

static void MrEdSchemeMessagesOutput(char *s, intptr_t l)
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

static intptr_t mrconsole_get_string(Scheme_Input_Port *ip,
				     char *buffer, intptr_t offset, intptr_t size,
				     int nonblock, Scheme_Object *unless)
{
  intptr_t result;
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;

  if (!pipe) return 0;
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

  if (!pipe) return NULL;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_progress_evt(pipe);
}

static int mrconsole_peeked_read(Scheme_Input_Port *ip,
				 intptr_t amount,
				 Scheme_Object *unless,
				 Scheme_Object *target_ch)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;

  if (!pipe) return 0;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_peeked_read(pipe, amount, unless, target_ch);
}

static int mrconsole_char_ready(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;

  if (!pipe) return 0;
  MrEdSchemeMessages("");

  init_console_in();
  pipe = console_inport;

  return scheme_char_ready(pipe);
}

static void mrconsole_close(Scheme_Input_Port *ip)
{
  Scheme_Object *pipe = (Scheme_Object *)ip->port_data;

  if (!pipe) return;

  init_console_in();
  pipe = console_inport;

  scheme_close_input_port(pipe);
}

static Scheme_Object *MrEdMakeStdIn(void)
{
  Scheme_Object *readp;
  Scheme_Input_Port *ip;

  if (scheme_get_place_id() == 0) {
    MZ_REGISTER_STATIC(stdin_pipe);

    scheme_pipe(&readp, &stdin_pipe);
  } else {
    /* for a non-main place, the port will be replaced anyway */
    readp = NULL;
  }

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

static intptr_t stdout_write(Scheme_Output_Port*op, const char *s, intptr_t d, intptr_t l, 
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

static intptr_t stderr_write(Scheme_Output_Port*op, const char *s, intptr_t d, intptr_t l, 
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

/* ---------------------------------------- */
/*        single-instance detection         */
/* ---------------------------------------- */

static char *CreateUniqueName()
{
  char desktop[MAX_PATH], session[32], *together;
  int dlen, slen;

  {
    // Name should be desktop unique, so add current desktop name
    HDESK hDesk;
    ULONG cchDesk = MAX_PATH - 1;

    hDesk = GetThreadDesktop(GetCurrentThreadId());
    
    if (!GetUserObjectInformation( hDesk, UOI_NAME, desktop, cchDesk, &cchDesk))
      desktop[0] = 0;
    else
      desktop[MAX_PATH - 1]  = 0;
  }

  {
    // Name should be session unique, so add current session id
    HANDLE hToken = NULL;
    // Try to open the token (fails on Win9x) and check necessary buffer size
    if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &hToken)) {
      DWORD cbBytes = 0;
      
      if(!GetTokenInformation( hToken, TokenStatistics, NULL, cbBytes, &cbBytes ) 
	 && GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
	  PTOKEN_STATISTICS pTS;

	  pTS = (PTOKEN_STATISTICS)malloc(cbBytes);
	  
	  if(GetTokenInformation(hToken, TokenStatistics, (LPVOID)pTS, cbBytes, &cbBytes)) {
	    sprintf(session, "-%08x%08x-",
		    pTS->AuthenticationId.HighPart, 
		    pTS->AuthenticationId.LowPart);
	  } else
	    session[0] = 0;
	  free(pTS);
      } else {
	session[0] = 0;
      }
    } else
      session[0] = 0;
  }

  dlen = strlen(desktop);
  slen =  strlen(session);
  together = (char *)malloc(slen + dlen + 1);
  memcpy(together, desktop, dlen);
  memcpy(together + dlen, session, slen);
  together[dlen + slen] = 0;
  
  return together;
}

#define GRACKET_GUID "B2261834-D535-44dd-8511-A26FC8F97DD0"

static int wm_is_gracket;

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  int i, len, gl;
  DWORD w;
  char **argv, *v;
  COPYDATASTRUCT cd;
  DWORD_PTR result;
  LRESULT ok;

  ok = SendMessageTimeout(wnd, wm_is_gracket,
			  0, 0, 
			  SMTO_BLOCK |
			  SMTO_ABORTIFHUNG,
			  200,
			  &result);

  printf("try %p result %d\n", wnd, result);

  if (ok == 0)
    return TRUE; /* ignore and continue */
  if ((intptr_t)result == 79) {
    /* found it */
  } else
    return TRUE; /* continue search */

  /* wnd is owned by another instance of this application */

  SetForegroundWindow(wnd);
  if (IsIconic(wnd)) 
    ShowWindow(wnd, SW_RESTORE);

  argv = (char **)param;
  
  len = gl = strlen(GRACKET_GUID);
  len += 4 + sizeof(DWORD);
  for (i = 1; argv[i]; i++) {
    len += sizeof(DWORD) + strlen(argv[i]);
  }
  w = i - 1;

  v = (char *)malloc(len);
  memcpy(v, GRACKET_GUID, gl);
  memcpy(v + gl, "OPEN", 4);
  memcpy(v + gl + 4, &w, sizeof(DWORD));
  len = gl + 4 + sizeof(DWORD);
  for (i = 1; argv[i]; i++) {
    w = strlen(argv[i]);
    memcpy(v + len, &w, sizeof(DWORD));
    len += sizeof(DWORD);
    memcpy(v + len, argv[i], w);
    len += w;
  }

  cd.dwData = 79;
  cd.cbData = len;
  cd.lpData = v;

  SendMessage(wnd, WM_COPYDATA, (WPARAM)wnd, (LPARAM)&cd);

  free(v);

  return FALSE;
}

static int CheckSingleInstance(char *normalized_path, char **argv)
{
  /* Check for an existing instance: */
  if (check_for_another[0] != 'n') {
    int alreadyrunning;
    HANDLE mutex;
    int j, l, i;
    char *a, *b;

    /* This mutex creation synchronizes multiple instances of
       the application that may have been started. */
    j = strlen(normalized_path);
    
    b = CreateUniqueName();
    l = strlen(b);
    a = (char *)malloc(j + l + 50);
    memcpy(a, normalized_path, j);
    for (i = 0; i < j; i++) {
      /* backslashes are not allowed in mutex names */
      if (a[i] == '\\') a[i] = '/';
    }
    memcpy(a + j, b, l);
    memcpy(a + j + l, "GRacket-" GRACKET_GUID, strlen(GRACKET_GUID) + 9);
    mutex = CreateMutex(NULL, FALSE, a);
    alreadyrunning = (GetLastError() == ERROR_ALREADY_EXISTS || 
		      GetLastError() == ERROR_ACCESS_DENIED);
    /* The call fails with ERROR_ACCESS_DENIED if the Mutex was 
       created in a different users session because of passing
       NULL for the SECURITY_ATTRIBUTES on Mutex creation. */
    wm_is_gracket = RegisterWindowMessage(a);
    free(a);

    if (alreadyrunning) {
      /* If another instance has been started, try to find it. */
      if (!EnumWindows((WNDENUMPROC)CheckWindow, (LPARAM)argv)) {
	return 1;
      }
    }
  }

  return 0;
}

static void pre_filter_cmdline_arguments(int *argc, char ***argv)
{
  scheme_register_process_global("PLT_WM_IS_GRACKET", (void *)(intptr_t)wm_is_gracket);
  scheme_register_process_global("PLT_GRACKET_GUID", GRACKET_GUID);
}

/* ---------------------------------------- */
/*           command-line parsing           */
/* ---------------------------------------- */

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int j, argc;
  char **argv, *normalized_path;

  load_delayed();

  {
    HANDLE h;
    h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h && (h != INVALID_HANDLE_VALUE)
        && (GetFileType(h) != FILE_TYPE_UNKNOWN)) {
      wx_in_terminal = 1;
    }
  }

  argv = cmdline_to_argv(&argc, &normalized_path);

  if (CheckSingleInstance(normalized_path, argv))
    return 0;

  if (!wx_in_terminal) {
    scheme_set_stdio_makers(MrEdMakeStdIn,
			    MrEdMakeStdOut,
			    MrEdMakeStdErr);
    scheme_set_console_output(MrEdSchemeMessagesOutput);
  }
  scheme_set_console_printf(MrEdSchemeMessages);
  scheme_set_exit(MrEdExit);

  j = MAIN(argc, argv);

  MrEdExit(j);
  /* shouldn't get here */

  return j;
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

#endif

/***********************************************************************/
/*                      X11 flag handling                              */
/***********************************************************************/

#ifdef wx_xt

typedef struct {
  char *flag;
  int arg_count;
} X_flag_entry;

#define SINGLE_INSTANCE "-singleInstance"

X_flag_entry X_flags[] = {
  { "-display", 1 },
  { "-geometry", 1 },
  { "-bg", 1 },
  { "-background", 1 },
  { "-fg", 1 },
  { "-foreground", 1 },
  { "-fn", 1 },
  { "-font", 1 },
  { "-iconic", 0 },
  { "-name", 1 },
  { "-rv", 0 },
  { "-reverse", 0 },
  { "+rv", 0 },
  { "-selectionTimeout", 1 },
  { "-synchronous", 0 },
  { "-title", 1 },
  { "-xnllanguage", 1 },
  { "-xrm", 1 },
  { SINGLE_INSTANCE, 0},
  { NULL, 0 }
};

static int filter_x_readable(char **argv, int argc)
  XFORM_SKIP_PROC
{
  int pos = 1, i;

  while (pos < argc) {
    for (i = 0; X_flags[i].flag; i++) {
      if (!strcmp(X_flags[i].flag, argv[pos]))
	break;
    }

    if (!X_flags[i].flag)
      return pos;
    else {
      int newpos = pos + X_flags[i].arg_count + 1;
      if (newpos > argc) {
	printf("%s: X Window System flag \"%s\" expects %d arguments, %d provided\n",
	       argv[0], argv[pos], X_flags[i].arg_count, argc - pos - 1);
	exit(-1);
      }
      pos = newpos;
    }
  }

  return pos;
}

static void pre_filter_cmdline_arguments(int *argc, char ***argv)
  XFORM_SKIP_PROC
{
  int pos;
  char **naya;

  pos = filter_x_readable(*argv, *argc);
  if (pos > 1) {
    scheme_register_process_global("PLT_X11_ARGUMENT_COUNT", (void *)(intptr_t)pos);
    scheme_register_process_global("PLT_X11_ARGUMENTS", *argv);
    naya = malloc((*argc - (pos - 1)) * sizeof(char *));
    memcpy(naya, *argv + (pos - 1), (*argc - (pos - 1)) * sizeof(char *));
    naya[0] = (*argv)[0];
    *argv = naya;
    *argc -= (pos - 1);
  }
}

#endif

/***********************************************************************/
/*                   Mac OS X flag handling                            */
/***********************************************************************/

#ifdef wx_mac

static void pre_filter_cmdline_arguments(int *argc, char ***argv)
  XFORM_SKIP_PROC
{
  if ((*argc > 1) && !strncmp((*argv)[1], "-psn_", 5)) {
    /* Finder adds "-psn_" when you double-click on the application.
       Drop it. */
    char **new_argv;
    new_argv = (char **)malloc(((*argc) - 1) * sizeof(char *));
    new_argv[0] = (*argv)[0];
    memcpy(new_argv + 1, (*argv) + 2, ((*argc) - 2) * sizeof(char *));
    (*argc)--;
    *argv = new_argv;
  }
  scheme_register_process_global("PLT_IS_FOREGROUND_APP", (void *)(intptr_t)0x1);
}

#endif

