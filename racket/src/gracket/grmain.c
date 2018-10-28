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

#ifdef wx_xt
# define PRE_FILTER_CMDLINE_ARGUMENTS
static void pre_filter_cmdline_arguments(int *argc, char ***argv);
#endif

#ifdef wx_mac
# define PRE_FILTER_CMDLINE_ARGUMENTS
static void pre_filter_cmdline_arguments(int *argc, char ***argv);
#endif

#define UNIX_INIT_FILENAME "~/.gracketrc"
#define WINDOWS_INIT_FILENAME "<home-dir>\\gracketrc.rktl"
#define INIT_FILENAME_CONF_SYM "gui-interactive-file"
#define DEFAULT_INIT_MODULE "racket/gui/interactive"
#define USER_INIT_MODULE "gui-interactive.rkt"
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

# include "../start/win_single.inc"

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

#include "../start/gui_filter.inc"
