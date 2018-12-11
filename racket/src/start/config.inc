/* This code fragment embeds strings in an executable that can be
   updated with various Racket exe-manipulation tools. */

#pragma GCC diagnostic ignored "-Wwrite-strings"

#if defined(__GNUC__)
# define PRESERVE_IN_EXECUTABLE __attribute__((used))
#else
# define PRESERVE_IN_EXECUTABLE /* empty */
#endif

PRESERVE_IN_EXECUTABLE
char * volatile scheme_cmdline_exe_hack = (char *)
  ("[Replace me for EXE hack                                       "
   "                                                              ]");

#if defined(MZ_CHEZ_SCHEME)
# define GC_PRECISION_TYPE "s"
#elif defined(MZ_PRECISE_GC)
# define GC_PRECISION_TYPE "3"
#else
# define GC_PRECISION_TYPE "c"
#endif
PRESERVE_IN_EXECUTABLE
char * volatile scheme_binary_type_hack = "bINARy tYPe:" INITIAL_BIN_TYPE GC_PRECISION_TYPE;
/* The format of bINARy tYPe is e?[zr]i[3cs].
   e indicates a starter executable
   z/r indicates Racket or GRacket
   i indicates ???
   3/c/s indicates 3m or CGC or Chez Scheme */

#ifndef INITIAL_COLLECTS_DIRECTORY
# ifdef DOS_FILE_SYSTEM
#  define INITIAL_COLLECTS_DIRECTORY "collects"
# else
#  define INITIAL_COLLECTS_DIRECTORY "../collects"
# endif
#endif

PRESERVE_IN_EXECUTABLE
char * volatile scheme_coldir = "coLLECTs dIRECTORy:" /* <- this tag stays, so we can find it again */
                       INITIAL_COLLECTS_DIRECTORY 
                       "\0\0" /* <- 1st nul terminates path, 2nd terminates path list */
                       /* Pad with at least 1024 bytes: */
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
                       "****************************************************************";
static int _coldir_offset = 19; /* Skip permanent tag */

#ifndef INITIAL_CONFIG_DIRECTORY
# ifdef DOS_FILE_SYSTEM
#  define INITIAL_CONFIG_DIRECTORY "etc"
# else
#  define INITIAL_CONFIG_DIRECTORY "../etc"
# endif
#endif

PRESERVE_IN_EXECUTABLE
char * volatile scheme_configdir = "coNFIg dIRECTORy:" /* <- this tag stays, so we can find it again */
                       INITIAL_CONFIG_DIRECTORY
                       "\0"
                       /* Pad with at least 1024 bytes: */
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
                       "****************************************************************";
static int _configdir_offset = 17; /* Skip permanent tag */

#ifndef MZ_XFORM
# define GC_CAN_IGNORE /**/
#endif

#ifndef MZ_PRECISE_GC
# define XFORM_OK_PLUS +
#endif

#ifdef OS_X
# include <mach-o/getsect.h>
# include <mach-o/dyld.h>
# include <fcntl.h>
#endif

#ifdef DOS_FILE_SYSTEM
# include <windows.h>

#ifndef DLL_RELATIVE_PATH
# define DLL_RELATIVE_PATH L"lib"
#endif
#include "delayed.inc"

static wchar_t *extract_dlldir()
{
  if (_dlldir[_dlldir_offset] != '<')
    return _dlldir + _dlldir_offset;
  else
    return NULL;
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif
#endif

#ifdef OS_X
static long get_segment_offset()
{
# if defined(__x86_64__) || defined(__arm64__)
  const struct segment_command_64 *seg;
# else
  const struct segment_command *seg;
#endif
  seg = getsegbyname("__PLTSCHEME");
  if (seg)
    return seg->fileoff;
  else
    return 0;
}
#endif

#ifdef DOS_FILE_SYSTEM
wchar_t *get_self_executable_path() XFORM_SKIP_PROC
{
  wchar_t *path;
  DWORD r, sz = 1024;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      free(path);
      sz = 2 * sz;
    } else
      break;
  }

  return path;
}

static DWORD find_by_id(HANDLE fd, DWORD rsrcs, DWORD pos, int id, long delta) XFORM_SKIP_PROC
{
  DWORD got, val;
  WORD name_count, id_count;
  
  SetFilePointer(fd, pos + 12 + delta, 0, FILE_BEGIN);
  ReadFile(fd, &name_count, 2, &got, NULL);
  ReadFile(fd, &id_count, 2, &got, NULL);

  pos += 16 + (name_count * 8);
  while (id_count--) {
    ReadFile(fd, &val, 4, &got, NULL);
    if (val == id) {
      ReadFile(fd, &val, 4, &got, NULL);
      return rsrcs + (val & 0x7FFFFFF);
    } else {
      ReadFile(fd, &val, 4, &got, NULL);
    }
  }

  return 0;
}

static long find_resource_offset(wchar_t *path, int id, long delta) XFORM_SKIP_PROC
{
  /* Find the resource of type `id` */
  HANDLE fd;

  fd = CreateFileW(path, GENERIC_READ,
                   FILE_SHARE_READ | FILE_SHARE_WRITE,
                   NULL,
                   OPEN_EXISTING,
                   0,
                   NULL);

  if (fd == INVALID_HANDLE_VALUE)
    return 0;
  else {
    DWORD val, got, sec_pos, virtual_addr, rsrcs, pos;
    WORD num_sections, head_size;
    char name[8];
    
    SetFilePointer(fd, 60+delta, 0, FILE_BEGIN);
    ReadFile(fd, &val, 4, &got, NULL);
    SetFilePointer(fd, val+4+2+delta, 0, FILE_BEGIN); /* Skip "PE\0\0" tag and machine */
    ReadFile(fd, &num_sections, 2, &got, NULL);
    SetFilePointer(fd, 12, 0, FILE_CURRENT); /* time stamp + symbol table */
    ReadFile(fd, &head_size, 2, &got, NULL);

    sec_pos = val+4+20+head_size;
    while (num_sections--) {
      SetFilePointer(fd, sec_pos+delta, 0, FILE_BEGIN);
      ReadFile(fd, &name, 8, &got, NULL);
      if ((name[0] == '.')
          && (name[1] == 'r')
          && (name[2] == 's')
          && (name[3] == 'r')
          && (name[4] == 'c')
          && (name[5] == 0)) {
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip virtual size */
        ReadFile(fd, &virtual_addr, 4, &got, NULL);
        SetFilePointer(fd, 4, 0, FILE_CURRENT); /* skip file size */
        ReadFile(fd, &rsrcs, 4, &got, NULL);
        SetFilePointer(fd, rsrcs+delta, 0, FILE_BEGIN);
        
        /* We're at the resource table; step through 3 layers */
        pos = find_by_id(fd, rsrcs, rsrcs, id, delta);
	if (pos) {
	  pos = find_by_id(fd, rsrcs, pos, 1, delta);
	  if (pos) {
	    pos = find_by_id(fd, rsrcs, pos, 1033, delta);

	    if (pos) {
	      /* pos is the reource data entry */
	      SetFilePointer(fd, pos+delta, 0, FILE_BEGIN);
	      ReadFile(fd, &val, 4, &got, NULL);
	      pos = val - virtual_addr + rsrcs;

	      CloseHandle(fd);

	      return pos+delta;
	    }
	  }
	}

	break;
      }
      sec_pos += 40;
    }

    /* something went wrong */
    CloseHandle(fd);
    return 0;
  }
}

static long get_segment_offset() XFORM_SKIP_PROC
{
  return find_resource_offset(get_self_executable_path(), 257, 0);
}

#endif

static void extract_built_in_arguments(char **_prog, char **_sprog, int *_argc, char ***_argv)
{
  GC_CAN_IGNORE char *prog = *_prog;
  GC_CAN_IGNORE char *sprog = *_sprog;
  
#ifdef DOS_FILE_SYSTEM
  {
    /* For consistency, strip trailing spaces and dots, and make sure the .exe
       extension is present. */
    int l = strlen(prog);
    if ((l > 0) && ((prog[l-1] == ' ') || (prog[l-1] == '.'))) {
      char *s;
      while ((l > 0) && ((prog[l-1] == ' ') || (prog[l-1] == '.'))) {
	l--;
      }
      s  = (char *)malloc(l + 1);
      memcpy(s, prog, l);
      s[l] = 0;
      prog = s;
    }
    if (l <= 4 
	|| (prog[l - 4] != '.')
	|| (tolower(((unsigned char *)prog)[l - 3]) != 'e')
	|| (tolower(((unsigned char *)prog)[l - 2]) != 'x')
	|| (tolower(((unsigned char *)prog)[l - 1]) != 'e')) {
      char *s;
      s  = (char *)malloc(l + 4 + 1);
      memcpy(s, prog, l);
      memcpy(s + l, ".exe", 5);
      prog = s;
    }
  }
#endif

  /* If scheme_cmdline_exe_hack is changed, then we extract built-in
     arguments. */
  if (scheme_cmdline_exe_hack[0] != '[') {
    int argc = *_argc;
    GC_CAN_IGNORE char **argv = *_argv;
    int n, i;
    long d;
    GC_CAN_IGNORE unsigned char *p;
    GC_CAN_IGNORE unsigned char *orig_p;
    char **argv2;

    p = NULL;
#ifdef DOS_FILE_SYSTEM
    if ((scheme_cmdline_exe_hack[0] == '?')
	|| (scheme_cmdline_exe_hack[0] == '*')) {
      /* This is how we make launchers in Windows. The cmdline is
	 added as a resource of type 257. The long integer at
	 scheme_cmdline_exe_hack[4] says where the command line starts
	 with the source, and scheme_cmdline_exe_hack[8] says how long
	 the cmdline string is. It might be relative to the
	 executable. */
      HANDLE fd;
      wchar_t *path;

      path = get_self_executable_path();
      fd = CreateFileW(path, GENERIC_READ,
		       FILE_SHARE_READ | FILE_SHARE_WRITE,
		       NULL,
		       OPEN_EXISTING,
		       0,
		       NULL);
      if (fd == INVALID_HANDLE_VALUE)
	p = (unsigned char *)"\0\0\0";
      else {
	long start, len;
	DWORD got;
	start = *(long *)&scheme_cmdline_exe_hack[4];
	len = *(long *)&scheme_cmdline_exe_hack[8];
	start += get_segment_offset();
	p = (unsigned char *)malloc(len);
	SetFilePointer(fd, start, 0, FILE_BEGIN);
	ReadFile(fd, p, len, &got, NULL);
	CloseHandle(fd);
	if (got != len)
	  p = (unsigned char *)"\0\0\0";
	else if (scheme_cmdline_exe_hack[0] == '*') {
	  /* "*" means that the first item is argv[0] replacement: */
	  sprog = prog;
	  prog = (char *)p + 4;

	  if ((prog[0] == '\\')
	      || ((((prog[0] >= 'a') && (prog[0] <= 'z'))
		   || 	((prog[0] >= 'A') && (prog[0] <= 'Z')))
		  && (prog[1] == ':'))) {
	    /* Absolute path */
	  } else {
	    /* Make it absolute, relative to this executable */
	    int plen = strlen(prog);
	    int mlen, len;
	    char *s2, *p2;

	    /* UTF-8 encode path: */
	    for (len = 0; path[len]; len++) { }
	    mlen = scheme_utf8_encode((unsigned int *)path, 0, len,
				      NULL, 0,
				      1 /* UTF-16 */);
	    p2 = (char *)malloc(mlen + 1);
	    mlen = scheme_utf8_encode((unsigned int *)path, 0, len,
				      (unsigned char *)p2, 0,
				      1 /* UTF-16 */);

	    while (mlen && (p2[mlen - 1] != '\\')) {
	      mlen--;
	    }
	    s2 = (char *)malloc(mlen + plen + 1);
	    memcpy(s2, p2, mlen);
	    memcpy(s2 + mlen, prog, plen + 1);
	    prog = s2;
	  }

	  p += (p[0]
		+ (((long)p[1]) << 8)
		+ (((long)p[2]) << 16)
		+ (((long)p[3]) << 24)
		+ 4);
	}
      }
      free(path);
    }
#endif
#if defined(OS_X)
    if (scheme_cmdline_exe_hack[0] == '?') {
      long fileoff, cmdoff, cmdlen;
      int fd;
      fileoff = get_segment_offset();

      p = (unsigned char *)scheme_cmdline_exe_hack + 4;
      cmdoff = (p[0]
                + (((long)p[1]) << 8)
                + (((long)p[2]) << 16)
                + (((long)p[3]) << 24));
      cmdlen = (p[4]
                + (((long)p[5]) << 8)
                + (((long)p[6]) << 16)
                + (((long)p[7]) << 24));
      p = malloc(cmdlen);
      
      fd = open(_dyld_get_image_name(0), O_RDONLY);
      lseek(fd, fileoff + cmdoff, 0);
      read(fd, p, cmdlen);
      close(fd);
    }
#endif
     
    if (!p)
      p = (unsigned char *)scheme_cmdline_exe_hack + 1;

    /* Command line is encoded as a sequence of pascal-style strings;
       we use four whole bytes for the length, though, little-endian. */

    orig_p = p;

    n = 0;
    while (p[0] || p[1] || p[2] || p[3]) {
      n++;
      p += (p[0]
	    + (((long)p[1]) << 8)
	    + (((long)p[2]) << 16)
	    + (((long)p[3]) << 24)
	    + 4);
    }
    
    argv2 = (char **)malloc(sizeof(char *) * (argc + n));
    p = orig_p;
    for (i = 0; i < n; i++) {
      d = (p[0]
	   + (((long)p[1]) << 8)
	   + (((long)p[2]) << 16)
	   + (((long)p[3]) << 24));
      argv2[i] = (char *)p + 4;
      p += d + 4;
    }
    for (; i < n + argc; i++) {
      argv2[i] = argv[i - n];
    }
    argv = argv2;
    argc += n;


    *_argc = argc;
    *_argv = argv;
  }

  *_prog = prog;
  *_sprog = sprog;
}

static char *extract_coldir()
{
  return scheme_coldir + _coldir_offset;
}

static char *extract_configdir()
{
  return scheme_configdir XFORM_OK_PLUS _configdir_offset;
}

#if !defined(OS_X) && !defined(DOS_FILE_SYSTEM)
# define NO_GET_SEGMENT_OFFSET
#endif
