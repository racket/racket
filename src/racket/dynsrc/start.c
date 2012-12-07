/* Launcher program for Windows. */
/* Builds a Racket starter if MZSTART is defined. */
/* Builds a GRacket starter if MRSTART is defined. */
/* If neither is defined, MZSTART is auto-defined. */

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
# define GOSUBDIR L"\\"
# define GOEXE L"gracket"
# define sGOEXE "gracket"
# define WAITTILDONE 0
#endif

#ifdef MZSTART
# define GOSUBDIR L"\\"
# define GOEXE L"racket"
# define sGOEXE "racket"
# define WAITTILDONE 1
#endif

#define MAXCOMMANDLEN 1024
#define MAX_ARGS 100

#if defined(_MSC_VER) || defined(__MINGW32__)
# define MSC_IZE(x) _ ## x
#else
# define MSC_IZE(x) x
#endif
#define DUPLICATE_INPUT

/* Win command lines limited to 1024 chars, so 1024 chars for
   command tail is ample */

static wchar_t *input = 
  L"<Command Line: Replace This ************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"****************************************************************"
  L"***************************************************************>";

/* Win long filenames limited to 255 chars, so 254 chars for
   directory is ample */

static wchar_t *exedir = L"<Executable Directory: Replace This ********"
                      L"********************************************"
                      L"********************************************"
                      L"********************************************"
                      L"********************************************"
                      L"********************************************>";

static wchar_t *variant = L"<Executable Variant: Replace This>";

static int wc_strlen(const wchar_t *ws)
{
  int l;
  for (l = 0; ws[l]; l++) { }
  return l;
}

static void wc_strcpy(wchar_t *dest, const wchar_t *src)
{
  while (*src) {
    *dest = *src;
    dest++;
    src++;
  }
  *dest = 0;
}

static void wc_strcat(wchar_t *dest, const wchar_t *src)
{
  while (*dest)
    dest++;
  wc_strcpy(dest, src);
}

static wchar_t *protect(wchar_t *s)
{
  wchar_t *naya;
  int has_space = 0, has_quote = 0, was_slash = 0;

  for (naya = s; *naya; naya++) {
    if (((*naya < 128) && isspace(*naya)) || (*naya == '\'')) {
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
    wchar_t *p;
    int wrote_slash = 0;

    naya = (wchar_t *)malloc((wc_strlen(s) + 3 + 3*has_quote) * sizeof(wchar_t));
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

static int parse_command_line(int count, wchar_t **command, 
			      wchar_t *buf, int maxargs, int skip)

{
  wchar_t *parse, *created, *write;
  int findquote = 0;
    
  parse = created = write = buf;
  while (*parse) {
    while (*parse && (*parse < 128) && isspace(*parse)) parse++;
    while (*parse && ((*parse > 128) || !isspace(*parse) || findquote))	{
      if (*parse== '"') {
	findquote = !findquote;
      } else if (*parse== '\\') {
	wchar_t *next;
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
	  
    if (*created) {
      if (skip) {
	skip--;
      } else {
	command[count++] = created;
	if (count == maxargs)
	  return count;
      }
    }
    created = write;
  }

  return count;
}

static wchar_t *make_command_line(int argc, wchar_t **argv)
{
  int i, len = 0;
  wchar_t *r;

  for (i = 0; i < argc; i++) {
    len += wc_strlen(argv[i]) + 1;
  }
  r = (wchar_t *)malloc(len * sizeof(wchar_t));
  len = 0;
  for (i = 0; i < argc; i++) {
    int l = wc_strlen(argv[i]);
    if (len) r[len++] = ' ';
    memcpy(r + len, argv[i], l * sizeof(wchar_t));
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
static wchar_t *copy_string(wchar_t *s)
{
  int l = wc_strlen(s);
  wchar_t *d = (wchar_t *)malloc((l + 1) * sizeof(wchar_t));
  memcpy(d, s, (l + 1) * sizeof(wchar_t));
  return d;
}
#endif

#if defined(MRSTART) || defined(__MINGW32__)
# define USE_WINMAIN
#endif

#ifdef USE_WINMAIN
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
		     LPSTR m_lpCmdLine, int nCmdShow)
#else
int wmain(int argc_in, wchar_t **argv_in)
#endif
{
  wchar_t go[MAXCOMMANDLEN * 2];
  wchar_t *args[MAX_ARGS + 1];
  wchar_t *command_line; 
  int count, i, cl_len;
  struct MSC_IZE(stat) st;
  STARTUPINFOW si;
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
  count = parse_command_line(count, args, input, MAX_ARGS, 0);
  
  /* exedir can be relative to the current executable */
  if ((exedir[0] == '\\')
      || ((((exedir[0] >= 'a') && (exedir[0] <= 'z'))
	   || 	((exedir[0] >= 'A') && (exedir[0] <= 'Z')))
	  && (exedir[1] == ':'))) {
    /* Absolute path */
  } else {
    /* Make it absolute, relative to this executable */
    int plen;
    int mlen;
    wchar_t *s2, *path;

    path = (wchar_t *)malloc(1024 * sizeof(wchar_t));
    GetModuleFileNameW(NULL, path, 1024);

    plen = wc_strlen(exedir);
    mlen = wc_strlen(path);

    while (mlen && (path[mlen - 1] != '\\')) {
      mlen--;
    }
    s2 = (wchar_t *)malloc((mlen + plen + 1) * sizeof(wchar_t));
    memcpy(s2, path, mlen * sizeof(wchar_t));
    memcpy(s2 + mlen, exedir, (plen + 1) * sizeof(wchar_t));
    exedir = s2;
  }

  wc_strcpy(go, exedir);
  wc_strcat(go, GOSUBDIR);
  wc_strcat(go, GOEXE);
  wc_strcat(go, variant);
  wc_strcat(go, L".exe");

  if (_wstat(go, &st)) {
#ifdef USE_WINMAIN
    wchar_t errbuff[MAXCOMMANDLEN * 2];
    swprintf(errbuff,L"Can't find %s",go);
    MessageBoxW(NULL,errbuff,L"Error",MB_OK);
#else
    char errbuff[MAXCOMMANDLEN * 2];
    sprintf(errbuff,"Can't find %S\n",go);
    WriteStr(out,errbuff);
#endif
    exit(-1);
  }

  args[0] = go;

#ifdef USE_WINMAIN
  {
    wchar_t *buf;
    LPWSTR m_lpCmdLine;

    m_lpCmdLine = GetCommandLineW();

    buf = (wchar_t *)malloc((wc_strlen(m_lpCmdLine) + 1) * sizeof(wchar_t));
    memcpy(buf, m_lpCmdLine, (wc_strlen(m_lpCmdLine) + 1) * sizeof(wchar_t));
    count = parse_command_line(count, args, buf, MAX_ARGS, 1);
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
  
  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  
  command_line = make_command_line(count, args);

  cl_len = wc_strlen(command_line);
  if (cl_len > MAXCOMMANDLEN) {
#ifdef MRSTART
    wchar_t errbuff[MAXCOMMANDLEN * 2];
    swprintf(errbuff,L"Command line of %d characters exceeds %d characters: %.1024s",
	     cl_len, MAXCOMMANDLEN,command_line);
    MessageBoxW(NULL,errbuff,L"Error",MB_OK);
#else
    char errbuff[MAXCOMMANDLEN * 2];
    sprintf(errbuff,"Command line of %d characters exceeds %d characters: %.1024S\n",
	    cl_len, MAXCOMMANDLEN,command_line);
    WriteStr(out,errbuff);
#endif
    exit(-1);
  } 

  if (!CreateProcessW(go,
		      command_line,
		      NULL, NULL, TRUE,
		      0, NULL, NULL, &si, &pi)) {
    
#ifdef MRSTART
    MessageBoxW(NULL, L"Can't start " GOEXE, L"Error", MB_OK);
#else
    WriteStr(out, "Can't start " sGOEXE "\n");
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
