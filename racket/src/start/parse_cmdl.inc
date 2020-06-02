/* 

   Windows command-line parsing:

   This command-line parser is meant to be consistent with the MSVC
   library for MSVC 2008 and later. It's mostly used when compiling
   with MinGW, but it's also used for parsing arguments embedded in a
   starter executable.

   The parser was is based on Microsoft documentation plus the missing
   parsing rule reported at

     http://daviddeley.com/autohotkey/parameters/parameters.htm#WINCRULES

   To quote from that page:

     The missing undocumented rule has to do with how doubledouble quotes ("") are handled:
       [...]
       After 2008
        A double quote encountered outside a double quoted block starts a double quoted block.
        A double quote encountered inside a double quoted block:
            not followed by another double quote ends the double quoted block.
            followed immediately by another double quote (e.g. ""), a single double quote is
            added to the output, and the double quoted block continues.

   Note that the CommandLineToArgv() function from shell32 does not
   parse the same way, specially in its handling of quotes.

*/

/* Note: modifies `buf`, and result array points into `buf` */
static int parse_command_line(int pos, /* starting pos ifor output in `_command` */
                              wchar_t ***_command, /*  initially `_command_len` in size */
                              int *_command_len, /* returns number of allocated slots */
                              wchar_t *buf, /* command line to parse */
                              int skip_exe) /* handle an executable name (which is parsed differently)? */
{
  wchar_t *parse, *created, *write;
  int maxargs = *_command_len;
  int findquote = 0; /* i.e., inside a quoted block? */
  wchar_t **command = *_command;

  if (maxargs == 0) {
    maxargs = 49;
    command = (wchar_t **)malloc((maxargs + 1) * sizeof(wchar_t *));
  }
  
  parse = created = write = buf;
  while (*parse) {
    int did_create = 0;
    while (*parse && (*parse < 128) && isspace(*parse)) parse++;
    while (*parse && ((*parse > 128) || !isspace(*parse) || findquote))	{
      if (*parse== '"') {
        if (!skip_exe && findquote && (parse[1] == '"')) {
          parse++;
          *(write++) = '"';
        } else {
          findquote = !findquote;
          did_create = 1;
        }
      } else if (!skip_exe && *parse== '\\') {
	wchar_t *next;
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
      if (skip_exe > 0) {
        --skip_exe;
      } else {
        command[pos++] = (wchar_t *)created;
        if (pos == maxargs) {
          wchar_t **c2;
          c2 = (wchar_t **)malloc(((2 * maxargs) + 1) * sizeof(wchar_t *));
          memcpy(c2, command, maxargs * sizeof(wchar_t *));
          maxargs *= 2;
        }
      }
    }
    created = write;
  }

  command[pos] = NULL;
  *_command = command;
  *_command_len = maxargs;

  return pos;
}
