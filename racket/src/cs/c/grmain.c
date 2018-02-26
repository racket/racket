#define do_pre_filter_cmdline_arguments(argc, argv) pre_filter_cmdline_arguments(argc, argv)
static void pre_filter_cmdline_arguments(int *argc, char ***argv);

#define INITIAL_BIN_TYPE "ri"
#define RACKET_IS_GUI 1

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
