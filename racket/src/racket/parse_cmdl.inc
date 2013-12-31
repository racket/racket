/* Windows command-line parsing */

#ifndef ASSUME_ASCII_COMMAND_LINE
static char *wchar_to_char(wchar_t *wa, int len)
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
#endif

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
    int did_create = 0;
    while (*parse && isspace(*parse)) { parse++; }
    while (*parse && (!isspace(*parse) || findquote))	{
      if (*parse== '"') {
	findquote = !findquote;
	did_create = 1;
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
	} else
	  *(write++) = *parse;
      } else
	*(write++) = *parse;
      parse++;
    }
    if (*parse)
      parse++;
    *(write++) = 0;
    
    if (*created || did_create) {
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

static char **cmdline_to_argv(int *_argc, char **_normalized_path)
{
#ifndef ASSUME_ASCII_COMMAND_LINE
  LPWSTR m_lpCmdLine;
  int j, l;
#endif
  int argc;
  char *a, **argv, *normalized_path;

#ifdef ASSUME_ASCII_COMMAND_LINE
  a = GetCommandLine();
#else
  m_lpCmdLine = GetCommandLineW();
  for (j = 0; m_lpCmdLine[j]; j++) {
  }
  a = wchar_to_char(m_lpCmdLine, j);
#endif

  argc = parse_command_line(&argv, a);

#ifndef ASSUME_ASCII_COMMAND_LINE
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
#else
  normalized_path = "?";
#endif

  *_argc = argc;
  if (_normalized_path)
    *_normalized_path = normalized_path;

  return argv;
}
