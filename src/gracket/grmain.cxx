/*
 * File:        mred.cc
 * Purpose:     GRacket main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 2004-2010 PLT Scheme Inc.
 * Copyright:   (c) 1995-2000, Matthew Flatt
 */

/* #define STANDALONE_WITH_EMBEDDED_EXTENSION */
/*    STANDALONE_WITH_EMBEDDED_EXTENSION builds an executable with
      built-in extensions. The extension is initialized by calling
      scheme_initialize(env), where `env' is the initial environment.
      By default, command-line parsing, the REP, and initilization
      file loading are turned off. */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
# define DONT_PARSE_COMMAND_LINE
# define DONT_RUN_REP
# define DONT_LOAD_INIT_FILE
#endif

/* wx_xt: */
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib
#define Uses_wxApp

#define IS_MRMAIN

#include "wx.h"

class wxStandardSnipClassList;
class wxBufferDataClassList;

#include "wxs/wxscheme.h"
#include "wxs/wxsgracket.h"

#include "gracket.h"

#ifdef wx_mac
extern char *wx_original_argv_zero;
extern short wxMacDisableMods;
extern long wxMediaCreatorId;
# include "simpledrop.h"
# ifdef OS_X
extern int wx_in_terminal;
# else
int wx_in_terminal; /* dummy */
# endif
#endif

#ifdef wx_msw
/* Hack: overwrite "y" with "n" in binary to disable checking for another
   instance of the same app. */
char *check_for_another = "yes, please check for another";
#endif

static void yield_indefinitely()
{
#ifdef MZ_PRECISE_GC
  void *dummy;
#endif
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;

  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    mred_wait_eventspace();
  }

  p->error_buf = save;

#ifdef MZ_PRECISE_GC
  dummy = NULL; /* makes xform think that dummy is live, so we get a __gc_var_stack__ */
#endif
}

#ifndef DONT_LOAD_INIT_FILE
static char *get_init_filename(Scheme_Env *env)
{
  Scheme_Object *type;
  Scheme_Object *path;

  type = scheme_intern_symbol("init-file");
  
  path = wxSchemeFindDirectory(1, &type);

  return SCHEME_BYTE_STR_VAL(path);
}
#endif

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
extern "C" Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifdef wx_x
# define INIT_FILENAME "~/.gracketrc"
#else
# ifdef wx_msw
#  define INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\gracketrc.rktl"
# else
#  ifdef OS_X
#   define INIT_FILENAME "~/.gracketrc"
#  else
#   define INIT_FILENAME "PREFERENCES:gracketrc.rktl"
#  endif
# endif
#endif
#define GET_INIT_FILENAME get_init_filename
#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
# define PRINTF mred_console_printf
# define CMDLINE_FFLUSH(x) /* nothing */
static void (*mred_console_printf)(char *str, ...);
# define NEED_MRED_CONSOLE_PRINTF
#else
# define PRINTF printf
# define CMDLINE_FFLUSH fflush
#endif
#define PROGRAM "GRacket"
#define PROGRAM_LC "gracket"
#define INITIAL_BIN_TYPE "ri"

#ifdef wx_mac
# ifndef OS_X
#  define GET_PLTCOLLECTS_VIA_RESOURCES
# endif
#endif

#ifdef GET_PLTCOLLECTS_VIA_RESOURCES
extern char *scheme_getenv_hack;
extern char *scheme_getenv_hack_value;
static char *pltcollects_from_resource;
#define SETUP_GETENV_HACK (scheme_getenv_hack = "PLTCOLLECTS", scheme_getenv_hack_value = pltcollects_from_resource);
#define TAKEDOWN_GETENV_HACK (scheme_getenv_hack = NULL, scheme_getenv_hack_value = NULL);
#else
#define SETUP_GETENV_HACK /* empty */
#define TAKEDOWN_GETENV_HACK /* empty */
#endif

#define CMDLINE_STDIO_FLAG
#define YIELD_BEFORE_EXIT
#define INITIAL_NAMESPACE_MODULE "racket/gui/init"

# include "../racket/cmdline.inc"

#ifdef wx_x
#if INTERRUPT_CHECK_ON
static int interrupt_signal_received;

static void interrupt(int)
{
  interrupt_signal_received = 1;

  signal(SIGINT, interrupt);
}
#endif
#endif

static FinishArgs *xfa;

static void do_graph_repl(Scheme_Env *env)
{
  mz_jmp_buf * volatile save, newbuf;
  Scheme_Thread * volatile p;
  Scheme_Object *a[2], *v;

  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    if (xfa->a->alternate_rep) {
      a[0] = scheme_intern_symbol("mred/mred");
      a[1] = scheme_intern_symbol("textual-read-eval-print-loop");
    } else {
      a[0] = scheme_intern_symbol("mred/mred");
      a[1] = scheme_intern_symbol("graphical-read-eval-print-loop");
    }
    v = scheme_dynamic_require(2, a);
    scheme_apply(v, 0, NULL);
  }

  p->error_buf = save;

#ifdef MZ_PRECISE_GC
  env = NULL; /* makes xform think that env is live, so we get a __gc_var_stack__ */
#endif
}

static int finish_cmd_line_run(void)
{
  return finish_cmd_line_run(xfa, do_graph_repl);
}

static int do_main_loop(FinishArgs *fa)
{
  wxREGGLOB(xfa);
  xfa = fa;

#ifdef wx_mac
  if (!fa->a->no_front) {
    ProcessSerialNumber psn;
    GetCurrentProcess(&psn);    
    SetFrontProcess(&psn); /* kCurrentProcess doesn't work */
  }
#endif

 {
   mz_jmp_buf * volatile save, newbuf;
   Scheme_Thread * volatile p;

   p = scheme_get_current_thread();
   save = p->error_buf;
   p->error_buf = &newbuf;
   if (!scheme_setjmp(newbuf))
     wxDoMainLoop();
   p->error_buf = save;
 }

  return 0;
}

static void run_from_cmd_line(int argc, char **argv, Scheme_Env *(*mk_basic_env)(void))
{
#ifdef NEED_MRED_CONSOLE_PRINTF
  mred_console_printf = scheme_get_console_printf();
#endif
  run_from_cmd_line(argc, argv, mk_basic_env, do_main_loop);
}

static int main_after_stack(int argc, char *argv[])
{
  int rval;

#ifdef wx_x
# if INTERRUPT_CHECK_ON
  signal(SIGINT, interrupt);
# endif
#endif

#ifdef SGC_STD_DEBUGGING
  fprintf(stderr, "Starting GRacket with sgc for debugging\n");
#endif

#ifdef wx_mac
  wxMacDisableMods = controlKey;
# ifndef OS_X
  scheme_creator_id = 'mReD';
  wxMediaCreatorId = 'mReD';
# endif
#endif

#ifdef wx_mac
  /* initialize Mac stuff */
  wx_original_argv_zero = argv[0];
  wxDrop_GetArgs(&argc, &argv, &wx_in_terminal);
#endif

  mred_set_run_from_cmd_line(run_from_cmd_line);
  mred_set_finish_cmd_line_run(finish_cmd_line_run);

  wxCreateApp();
  
  rval = wxEntry(argc, argv);

#ifdef wx_msw
  mred_clean_up_gdi_objects();
#endif	
  
  return rval;
}

/* **************************************************************** */
/*   Main for Unix and Mac OS X                                     */
/* **************************************************************** */

/* Just jumps to generic main. */

#ifndef wx_msw 

typedef struct {
  int argc;
  char **argv;
} Main_Args;

static int call_main_after_stack(void *data)
{
  Main_Args *ma = (Main_Args *)data;
  return main_after_stack(ma->argc, ma->argv);
}

int main(int argc, char *argv[])
  XFORM_SKIP_PROC
{
  Main_Args ma;
  ma.argc = argc;
  ma.argv = argv;
  return scheme_main_stack_setup(1, call_main_after_stack, &ma);
}

#endif

/* **************************************************************** */
/*   Main for Windows                                               */
/* **************************************************************** */

/* Implements single-instance mode and otherwise initializes Windows. */

#ifdef wx_msw 

/* Some of the code to avoid multiple application instances is taken from
    http://www.codeproject.com/cpp/avoidmultinstance.asp */

static int wm_is_mred;

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

static BOOL CALLBACK CheckWindow(HWND wnd, LPARAM param)
{
  int i, len, gl;
  DWORD w;
  char **argv, *v;
  COPYDATASTRUCT cd;
  DWORD result;
  LRESULT ok;

  ok = SendMessageTimeout(wnd, wm_is_mred,
			  0, 0, 
			  SMTO_BLOCK |
			  SMTO_ABORTIFHUNG,
			  200,
			  &result);

  if (ok == 0)
    return TRUE; /* ignore and continue */
  if (result == 79) {
    /* found it */
  } else
    return TRUE; /* continue search */

  /* wnd is owned by another instance of this application. */

  SetForegroundWindow(wnd);
  if (IsIconic(wnd)) 
    ShowWindow(wnd, SW_RESTORE);

  argv = (char **)param;
  
  len = gl = strlen(MRED_GUID);
  len += 4 + sizeof(DWORD);
  for (i = 1; argv[i]; i++) {
    len += sizeof(DWORD) + strlen(argv[i]);
  }
  w = i - 1;

  v = (char *)malloc(len);
  memcpy(v, MRED_GUID, gl);
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

/* To propagate args from WinMain to wxWinMain via
   scheme_main_stack_setup: */
typedef struct {
  int wm_is_mred;
  HINSTANCE hInstance;
  HINSTANCE hPrevInstance;
  int argc;
  char **argv;
  int nCmdShow;
} WinMain_Args;

static int WinMain_after_stack(void *_wma)
{
  WinMain_Args *wma = (WinMain_Args *)_wma;

  return wxWinMain(wma->wm_is_mred, wma->hInstance, wma->hPrevInstance, 
                   wma->argc, wma->argv, 
                   wma->nCmdShow, 
                   main_after_stack);
}

int APIENTRY WinMain_dlls_ready(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  LPWSTR m_lpCmdLine;
  long argc, j, l;
  char *a, **argv, *b, *normalized_path = NULL;
  WinMain_Args wma;

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
  if (!normalized_path) {
    normalized_path = "???";
  } else {
    for (j = 0; normalized_path[j]; j++) {
      if (normalized_path[j] == '\\') {	
	normalized_path[j] = '/';
      }
    }
  }

  /* Check for an existing instance: */
  if (check_for_another[0] != 'n') {
    int alreadyrunning;
    HANDLE mutex;

    /* This mutex creation synchronizes multiple instances of
       the application that may have been started. */
    j = strlen(normalized_path);
    b = CreateUniqueName();
    l = strlen(b);
    a = (char *)malloc(j + l + 50);
    memcpy(a, normalized_path, j);
    memcpy(a + j, b, l);
    memcpy(a + j + l, "GRacket-" MRED_GUID, strlen(MRED_GUID) + 9);
    mutex = CreateMutex(NULL, FALSE, a);
    alreadyrunning = (GetLastError() == ERROR_ALREADY_EXISTS || 
		      GetLastError() == ERROR_ACCESS_DENIED);
    // The call fails with ERROR_ACCESS_DENIED if the Mutex was 
    // created in a different users session because of passing
    // NULL for the SECURITY_ATTRIBUTES on Mutex creation);
    wm_is_mred = RegisterWindowMessage(a);
    free(a);
    
    if (alreadyrunning) {
      /* If another instance has been started, try to find it. */
      if (!EnumWindows((WNDENUMPROC)CheckWindow, (LPARAM)argv)) {
	return 0;
      }
    }
  }

  wma.wm_is_mred = wm_is_mred;
  wma.hInstance = hInstance;
  wma.hPrevInstance = hPrevInstance;
  wma.argc = argc;
  wma.argv = argv;
  wma.nCmdShow = nCmdShow;

  return scheme_main_stack_setup(1, WinMain_after_stack, &wma);
}

#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
extern "C" long _tls_index;
static __declspec(thread) void *tls_space;
#endif

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  /* Order matters: load dependencies first */
# ifndef MZ_PRECISE_GC
  load_delayed_dll(NULL, "libmzgcxxxxxxx.dll");
# endif
  load_delayed_dll(NULL, "libracket" DLL_3M_SUFFIX "xxxxxxx.dll");
  load_delayed_dll(NULL, "libgracket" DLL_3M_SUFFIX "xxxxxxx.dll");
  record_dll_path();
# ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
  scheme_register_tls_space(&tls_space, _tls_index);
# endif

  return WinMain_dlls_ready(hInstance, hPrevInstance, ignored, nCmdShow);
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

#if _MSC_VER >= 1400
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

#endif
