/* This code fragment embeds strings in an executable that can be
   updated with various Racket exe-manipulation tools. 

   Used by CS's "main.c" and BC's "cmdline.inc".

   Functions that are "exported":

      self_exe_t get_self_path(char *argv0);
      void extract_built_in_arguments(const self_exe_t self_exe, 
                                      char **_prog, char **_sprog,
                                      int *_argc, char ***_argv);
      char *SELF_PATH_TO_BYTES(self_exe_t)

      char *extract_coldir();
      char *extract_configdir();

      long get_segment_offset(self_exe_t self); // added to `-k` arguments

    Windows:
      self_exe_t is wchar_t
      wchar_t *extract_dlldir();
      long find_resource_offset(const wchar_t *path, int id, long delta);
      char *string_to_utf8(wchar_t *p);

    Mac OS:
      long find_mach_o_segment(const char *name, long *_len);

    Unix:
      int find_elf_section_offset(const char *filename,
                                  const char *name,
                                  int *_start, int *_end);

*/

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

#ifdef DOS_FILE_SYSTEM
# include <windows.h>
#else
# include <errno.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/uio.h>
# include <unistd.h>
#endif

#include "self_exe.inc"

#ifdef DOS_FILE_SYSTEM
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
static long get_segment_offset(const char *me)
{
  return find_mach_o_segment("__PLTSCHEME", NULL);
}
#endif

#ifdef DOS_FILE_SYSTEM

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

static long find_resource_offset(const wchar_t *path, int id, long delta) XFORM_SKIP_PROC
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

static long get_segment_offset(const wchar_t *me) XFORM_SKIP_PROC
{
  return find_resource_offset(me, 257, 0);
}

static char *string_to_utf8(wchar_t *p) XFORM_SKIP_PROC
{
  char *r;
  int len;

  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, NULL, 0, NULL, NULL);
  r = malloc(len);
  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, r, len, NULL, NULL);

  return r;
}

# define SELF_PATH_TO_BYTES(s) string_to_utf8(s)

#else

# define SELF_PATH_TO_BYTES(s) s

#endif

#if !defined(OS_X) && !defined(DOS_FILE_SYSTEM)

static long get_segment_offset(const char *me) XFORM_SKIP_PROC
{
  int start = 0, end = 0;

  (void)find_elf_section_offset(me, ".rackprog", &start, &end);
  
  return start;
}

#endif

static void extract_built_in_arguments(const self_exe_t self_exe, char **_prog, char **_sprog, int *_argc, char ***_argv)
{
  GC_CAN_IGNORE char *prog = *_prog;
  GC_CAN_IGNORE char *sprog = *_sprog;
  
#ifdef DOS_FILE_SYSTEM
  /* Replacing `prog` with `self_exe` has the effect of stripping trailing spaces
     and dots, if they were in the command line, as well as adding an `.exe`
     if its wasn't alerady present. It also avoids the problem that WSL2 currently
     launches Windows programs without the path used to reference them. */
  prog = string_to_utf8(self_exe);
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
    if ((scheme_cmdline_exe_hack[0] == '?')
	|| (scheme_cmdline_exe_hack[0] == '*')) {
#ifdef DOS_FILE_SYSTEM
      /* This is how we make launchers in Windows. The cmdline is
	 added as a resource of type 257. The long integer at
	 scheme_cmdline_exe_hack[4] says where the command line starts
	 with the source, and scheme_cmdline_exe_hack[8] says how long
	 the cmdline string is. It might be relative to the
	 executable. */
      HANDLE fd;
      wchar_t *path = self_exe;

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
	start += get_segment_offset(self_exe);
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
	    int mlen;
	    char *s2, *p2;

	    /* UTF-8 encode path: */
            p2 = string_to_utf8(path);

	    for (mlen = 0; p2[mlen]; mlen++) { }
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
#else
      long fileoff, cmdoff, cmdlen, need, got;
      int fd;
      fileoff = get_segment_offset(self_exe);

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

      fd = open(self_exe, O_RDONLY);
      lseek(fd, fileoff + cmdoff, 0);

      for (need = cmdlen; need > 0; need -= got) {
	got = read(fd, p + (cmdlen - need), need);
	if (got == -1) {
	  got = 0;
	  if (errno != EINTR)
	    break;
	}
      }
      close(fd);

      if (scheme_cmdline_exe_hack[0] == '*') {
        /* "*" means that the first item is argv[0] replacement,
           because this executable is being treated as a launcher */
        sprog = prog;
        prog = (char *)p + 4;
        
        if (prog[0] == '/') {
          /* Absolute path */
        } else {
          /* Make it absolute, relative to this executable */
          int plen = strlen(prog);
          int mlen = strlen(self_exe);
          char *s2;

          while (mlen && (self_exe[mlen - 1] != '/')) {
            mlen--;
          }
          s2 = (char *)malloc(mlen + plen + 1);
          memcpy(s2, self_exe, mlen);
          memcpy(s2 + mlen, prog, plen + 1);
          prog = s2;
        }

        p += (p[0]
              + (((long)p[1]) << 8)
              + (((long)p[2]) << 16)
              + (((long)p[3]) << 24)
              + 4);
      }
#endif
    }

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
