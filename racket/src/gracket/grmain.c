#ifdef WIN32
/* Hack: overwrite "y" with "n" in binary to disable checking for another
   instance of the same app. */
char *check_for_another = "yes, please check for another";
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

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

# include "../start/win_single.inc"

static void pre_filter_cmdline_arguments(int *argc, char ***argv)
{
  scheme_register_process_global("PLT_WM_IS_GRACKET", (void *)(intptr_t)wm_is_gracket);
  scheme_register_process_global("PLT_GRACKET_GUID", GRACKET_GUID);
}

static void MrEdSchemeMessages(char *msg, ...)
{
  HANDLE console_out;
  va_list args;

  scheme_ensure_console_ready();
  
  va_start(args, msg);

  console_out = GetStdHandle(STD_OUTPUT_HANDLE);

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

  va_end(args);
}

static void MrEdSchemeMessagesOutput(char *s, intptr_t l)
{
  if (l)
    MrEdSchemeMessages(NULL, s, 0, l);
}

/* ---------------------------------------- */
/*           command-line parsing           */
/* ---------------------------------------- */

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int j, argc, in_terminal = 0;
  char **argv, *normalized_path;

  load_delayed();

  {
    HANDLE h;
    h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h && (h != INVALID_HANDLE_VALUE)
        && (GetFileType(h) != FILE_TYPE_UNKNOWN)) {
      in_terminal = 1;
    }
  }

  argv = cmdline_to_argv(&argc, &normalized_path);

  if (CheckSingleInstance(normalized_path, argv))
    return 0;

  if (!in_terminal)
    scheme_set_console_output(MrEdSchemeMessagesOutput);
  scheme_set_console_printf(MrEdSchemeMessages);

  j = MAIN(argc, argv);

  scheme_immediate_exit(j);
  /* shouldn't get here */

  return j;
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

#endif

#include "../start/gui_filter.inc"
