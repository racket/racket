#define do_pre_filter_cmdline_arguments(argc, argv) pre_filter_cmdline_arguments(argc, argv)
static void pre_filter_cmdline_arguments(int *argc, char ***argv);

#define INITIAL_BIN_TYPE "ri"
#define RACKET_IS_GUI 1

#if WIN32
# define DLL_RELATIVE_PATH L"."
# ifndef INITIAL_COLLECTS_DIRECTORY
#  define INITIAL_COLLECTS_DIRECTORY "../collects"
# endif
# ifndef INITIAL_CONFIG_DIRECTORY
#  define INITIAL_CONFIG_DIRECTORY "../etc"
# endif

/* Hack: overwrite "y" with "n" in binary to disable checking for another
   instance of the same app. */
char *check_for_another = "yes, please check for another";

# include <windows.h>
# include "../start/win_single.inc"
# define CHECK_SINGLE_INSTANCE
#endif

#include "main.c"

#if defined(WIN32)
static void pre_filter_cmdline_arguments(int *argc, char ***argv) { }
#elif defined(OS_X)
# define wx_mac
#else
# define wx_xt
#endif

static void scheme_register_process_global(const char *key, void *v)
{
#ifdef OS_X
  /* "PLT_IS_FOREGROUND_APP" is set in "main.sps" */
#endif
#ifdef wx_xt
  if (!strcmp(key, "PLT_X11_ARGUMENT_COUNT"))
    x11_arg_count = (int)(intptr_t)v;
  else if (!strcmp(key, "PLT_X11_ARGUMENTS")) {
    x11_args = malloc(32);
    sprintf(x11_args, "%p", v);
  }
#endif
}

#include "../../start/gui_filter.inc"
