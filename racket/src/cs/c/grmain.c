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

# include <Windows.h>
# include "../start/win_single.inc"
# define CHECK_SINGLE_INSTANCE
#endif

#include "main.c"

#ifdef OS_X
# define wx_mac
#else
# define wx_xt
#endif

static void scheme_register_process_global(const char *key, void *v)
{
}

#include "../../start/gui_filter.inc"
