#include "parse_cmdl.inc"

static char *wchar_to_char(wchar_t *wa, int len)
{
  char *r;
  int wlen;

  wlen = WideCharToMultiByte(CP_UTF8, 0, wa, len+1, NULL, 0, NULL, NULL);
  r = (char *)malloc(wlen);
  wlen = WideCharToMultiByte(CP_UTF8, 0, wa, len+1, r, wlen, NULL, NULL);

  return r;
}

static char **cmdline_to_argv(int *_argc, char **_normalized_path)
{
  LPWSTR m_lpCmdLine;
  int argc, j, l;
  wchar_t **w_argv = NULL;
  int argv_len = 0;
  char *a, **argv, *normalized_path;

  m_lpCmdLine = GetCommandLineW();

  argc = parse_command_line(1, &w_argv, &argv_len, m_lpCmdLine, 1);

  argv = (char **)malloc((argc + 1) * sizeof(char *));
  for (j = 1; j < argc; j++) {
    int len;
    for (len = 0; w_argv[j][len] != 0; len++);
    argv[j] = wchar_to_char(w_argv[j], len);
  }
  argv[argc] = NULL; /* CheckSingleInstance relies on this terminator */

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
	CharLowerBuffW(my_name, l);
	normalized_path = wchar_to_char(my_name, l);
	free(my_name);
	break;
      } else {
	free(my_name);
	name_len = name_len * 2;
      }
    }
  }

  *_argc = argc;
  if (_normalized_path)
    *_normalized_path = normalized_path;

  return argv;
}
