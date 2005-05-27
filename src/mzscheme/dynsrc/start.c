/* Launcher program for Windows. */
/* Builds a MzScheme starter if MZSTART is defined. */
/* Builds a MrEd starter if MRSTART is defined. */
/* If neither is defined, MZSTART is auto-defined */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <process.h>
#include <ctype.h>

#ifndef MRSTART
# ifndef MZSTART
#  define MZSTART
# endif
#endif

#ifdef MRSTART
# define GOSUBDIR "\\"
# define GOEXE "mred.exe"
# define GOEXE3M "mred3m.exe"
# define WAITTILLDONE 0
#endif

#ifdef MZSTART
# define GOSUBDIR "\\"
# define GOEXE "mzscheme.exe"
# define GOEXE3M "mzscheme3m.exe"
# define WAITTILDONE 1
#endif

#define MAXCOMMANDLEN 1024
#define MAX_ARGS 100

#if defined(_MSC_VER)
# define MSC_IZE(x) _ ## x
#else
# define MSC_IZE(x) x
#endif
#define DUPLICATE_INPUT

/* Win command lines limited to 1024 chars, so 1024 chars for
   command tail is ample */

static char *input = 
  "<Command Line: Replace This ************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "****************************************************************"
  "***************************************************************>";

/* Win long filenames limited to 255 chars, so 254 chars for
   directory is ample */

static char *exedir = "<Executable Directory: Replace This ********"
                      "********************************************"
                      "********************************************"
                      "********************************************"
                      "********************************************"
                      "********************************************>";

static char *variant = "<Executable Variant: Replace This>";

static char *protect(char *s)
{
  char *naya;
  int has_space = 0, has_quote = 0, was_slash = 0;

  for (naya = s; *naya; naya++) {
    if (isspace(*naya) || (*naya == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (*naya == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (*naya == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }

  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;

    naya = malloc(strlen(s) + 3 + 3*has_quote);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--)
	  *(p++) = '\\';
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    *(p++) = '"';
    *p = 0;

    return naya;
  }

  return s;
}

static int parse_command_line(int count, char **command, 
			      char *buf, int maxargs)

{
  char *parse, *created, *write;
  int findquote = 0;
    
  parse = created = write = buf;
  while (*parse) {
    while (*parse && isspace(*parse)) parse++;
    while (*parse && (!isspace(*parse) || findquote))	{
      if (*parse== '"') {
	findquote = !findquote;
      } else if (*parse== '\\') {
	char *next;
	for (next = parse; *next == '\\'; next++);
	if (*next == '"') {
	  /* Special handling: */
	  int count = (next - parse), i;
	  for (i = 1; i < count; i += 2)
	    *(write++) = '\\';
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
      command[count++] = created;
      if (count == maxargs)
	return count;
    }
    created = write;
  }

  return count;
}

static char *make_command_line(int argc, char **argv)
{
  int i, len = 0;
  char *r;

  for (i = 0; i < argc; i++) {
    len += strlen(argv[i]) + 1;
  }
  r = malloc(len);
  len = 0;
  for (i = 0; i < argc; i++) {
    int l = strlen(argv[i]);
    if (len) r[len++] = ' ';
    memcpy(r + len, argv[i], l);
    len += l;
  }
  r[len] = 0;
  return r;
}

#ifdef MZSTART
void WriteStr(HANDLE h, const char *s) {
  DWORD done;
  WriteFile(h, s, strlen(s), &done, NULL);
}
#endif

#ifdef DUPLICATE_INPUT
static char *copy_string(char *s)
{
  int l = strlen(s);
  char *d = malloc(l + 1);
  memcpy(d, s, l + 1);
  return d;
}
#endif

#ifdef MRSTART
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
		     LPSTR m_lpCmdLine, int nCmdShow)
#else
int main(int argc_in, char **argv_in)
#endif
{
  char *p;
  char go[MAXCOMMANDLEN * 2];
  char *args[MAX_ARGS + 1];
  char *command_line; 
  int count, i;
  struct MSC_IZE(stat) st;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
#ifdef MZSTART
  HANDLE out;

  out = GetStdHandle(STD_OUTPUT_HANDLE);
#endif
 
#ifdef DUPLICATE_INPUT
  /* gcc: input is read-only */
  input = copy_string(input);
  exedir = copy_string(exedir);
#endif

  count = 1;
  count = parse_command_line(count, args, input, MAX_ARGS);
  
  /* exedir should be PLTHOME path */
  strcpy(go, exedir);
  strcat(go, GOSUBDIR);
  strcat(go, (variant[0] != '<') ? GOEXE3M : GOEXE);

  if (_stat(go, &st)) {
    char errbuff[MAXCOMMANDLEN * 2];
#ifdef MRSTART
    sprintf(errbuff,"Can't find %s",go);
    MessageBox(NULL,errbuff,"Error",MB_OK);
#else
    sprintf(errbuff,"Can't find %s\n",go);
    WriteStr(out,errbuff);
#endif
    exit(-1);
  }

  args[0] = go;

#ifdef MRSTART
  {
    char *buf;
    
    buf = malloc(strlen(m_lpCmdLine) + 1);
    memcpy(buf, m_lpCmdLine, strlen(m_lpCmdLine) + 1);
    count = parse_command_line(count, args, buf, MAX_ARGS);
  }
#else
  {
    int i;
    for (i = 1; i < argc_in; i++)
      args[count++] = argv_in[i];
  }
#endif
  
  args[count] = NULL;
  
  for (i = 0; i < count; i++) {
    args[i] = protect(args[i]);
    /* MessageBox(NULL, args[i], "Argument", MB_OK); */
  }
  
  /* make the exedir PLTHOME if it's not currently */

  p = getenv("PLTHOME");
  if (p == NULL || strcmp(p,exedir)) {
    SetEnvironmentVariable("PLTHOME", exedir);
  }
  
  for (i = 0; i < sizeof(si); i++)
    ((char *)&si)[i] = 0;
  si.cb = sizeof(si);
  
  command_line = make_command_line(count, args);

  if (strlen(command_line) > MAXCOMMANDLEN) {
    char errbuff[MAXCOMMANDLEN * 2];
#ifdef MRSTART
    sprintf(errbuff,"Command line exceeds %d characters: %s",
	    MAXCOMMANDLEN,go);
    MessageBox(NULL,errbuff,"Error",MB_OK);
#else
    sprintf(errbuff,"Command line exceeds %d characters: %s\n",
	    MAXCOMMANDLEN,go);
    WriteStr(out,errbuff);
#endif
    exit(-1);
  } 

  if (!CreateProcess(go,
		     command_line,
		     NULL, NULL, TRUE,
		     0, NULL, NULL, &si, &pi)) {
    
#ifdef MRSTART
    MessageBox(NULL, "Can't start " GOEXE, "Error", MB_OK);
#else
    WriteStr(out, "Can't start " GOEXE "\n");
#endif
    return -1;
  } else {
#if WAITTILDONE
    DWORD result;
    WaitForSingleObject(pi.hProcess, INFINITE);
    GetExitCodeProcess(pi.hProcess, &result);
    return result;
#else
    return 0;
#endif
  }
}
