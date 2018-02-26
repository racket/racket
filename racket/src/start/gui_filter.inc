/*
  Defines 
     void pre_filter_cmdline_arguments(int *argc, char ***argv)
  and may use
     void scheme_register_process_global(const char *key, void *v);
*/
  
#if !defined(MZ_XFORM) && !defined(XFORM_SKIP_PROC)
# define XFORM_SKIP_PROC /**/
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
