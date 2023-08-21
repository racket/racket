/* Used by "ustart.c" and "config.inc".

   Defines self_exe_t get_self_path(char *exec_file),
   which takes argv[0] and returns an improved representation of
   the containing executable. At worst, on Unix, uses `PATH` to
   convert `exec_file` into a path.

   On Mac (even XonX), `find_mach_o_segment` is also defined,
   unless `SELF_EXE_NO_EXTRAS` is defined.

   On Unix (not Mac OS, not even XonX), `find_elf_section`
   is also defined. 

   If USE_EXE_LOOKUP_VIA_PATH, also: `lookup_exe_via_path`,
   `path_append`, and `do_path_append`.

   Parts of the implementaiton here are from the LLVM Project under
   the Apache License v2.0 with LLVM Exceptions.
*/

#define USE_GENERIC_GET_SELF_PATH

#if defined(DOS_FILE_SYSTEM) && !defined(SELF_EXE_WINDOWS_AS_UTF8)
typedef wchar_t *self_exe_t;
#else
typedef char *self_exe_t;
#endif

#if defined(__linux__) || defined(__GNU__)
# define USE_PROC_SELF_EXE_FILESYSTEM_PATH "/proc/self/exe"
#endif

/* From LLVM: (but removed OpenBSD, which doesn't have "/proc") */
#if defined(__NetBSD__) || defined(__minix) || \
    defined(__DragonFly__) || defined(__FreeBSD_kernel__) || defined(_AIX)
# define USE_PROC_SELF_EXE_FILESYSTEM_PATH "/proc/curproc/file"
#endif

#if defined(USE_PROC_SELF_EXE_FILESYSTEM_PATH)
# include <errno.h>
# include <unistd.h>
# define GENERIC_GET_SELF_PATH_NAME generic_get_self_path
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file);
static char *get_self_path(const char *exec_file)
{
  char *s;
  ssize_t len, blen = 256;

  s = malloc(blen);

  while (1) {
    len = readlink(USE_PROC_SELF_EXE_FILESYSTEM_PATH, s, blen-1);
    if (len == (blen-1)) {
      free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      /* possibly in a chroot environment where "/proc" is not
         available, so fall back to generic approach: */
      free(s);
      return generic_get_self_path(exec_file);
    } else
      break;
  }
  s[len] = 0;

#if defined(__GNU__)
  /* From LLVM comments: */
  /* [...] on GNU/Hurd, /proc/self/exe is a symlink to the path that was used to start
     the program, and not the eventual binary file. Therefore, call realpath [...] */
  {
    char *link_path;
    link_path = realpath(s, NULL);
    if (link_path) {
      free(s);
      return link_path;
    }
  }
#endif

  return s;
}
#endif

#if defined(__FreeBSD__)
# include <osreldate.h>
# if __FreeBSD_version >= 1300057
#  include <sys/auxv.h>
#  include <limits.h>
# else
#  include <machine/elf.h>
extern char **environ;
# endif
# define GENERIC_GET_SELF_PATH_NAME generic_get_self_path
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file);
static char *get_self_path(const char *exec_file)
{
  /* From LLVM, including most comments: */

  /* On FreeBSD if the exec path specified in ELF auxiliary vectors is
     preferred, if available.  /proc/curproc/file and the KERN_PROC_PATHNAME
     sysctl may not return the desired path if there are multiple hardlinks
     to the file. */
#if __FreeBSD_version >= 1300057
  char exe_path[PATH_MAX];
  if (elf_aux_info(AT_EXECPATH, exe_path, sizeof(exe_path)) == 0) {
    char *link_path;
    link_path = realpath(exe_path, NULL);
    if (link_path)
      return link_path;
  }
#else
  /* elf_aux_info(AT_EXECPATH, ... is not available in all supported versions,
     fall back to finding the ELF auxiliary vectors after the process's
     environment. */
  char **p = environ;
  while (*p++ != 0)
    ;
  /* Iterate through auxiliary vectors for AT_EXECPATH. */
  for (Elf_Auxinfo *aux = (Elf_Auxinfo *)p; aux->a_type != AT_NULL; aux++) {
    if (aux->a_type == AT_EXECPATH) {
      char *link_path;
      link_path = realpath((char *)aux->a_un.a_ptr, NULL);
      if (link_path)
        return link_path;
    }
  }
#endif
  /* Fallback: */
  return generic_get_self_path(exec_file);
}
#endif

#if defined(__APPLE__) && defined(__MACH__)
# include <mach-o/getsect.h>
# include <mach-o/dyld.h>
static char *get_self_path(const char *exec_file)
{
  char buf[1024], *s;
  uint32_t size = sizeof(buf);
  int r;
  
  r = _NSGetExecutablePath(buf, &size);
  if (!r)
    return strdup(buf);
  else {
    s = malloc(size);
    r = _NSGetExecutablePath(s, &size);
    if (!r)
      return s;
    return strdup(exec_file);
  }
}
# undef USE_GENERIC_GET_SELF_PATH

# ifndef SELF_EXE_NO_EXTRAS
static long find_mach_o_segment(const char *name, long *_len)
{
#  if defined(__x86_64__) || defined(__arm64__)
  const struct segment_command_64 *seg;
#  else
  const struct segment_command *seg;
#  endif
  seg = getsegbyname(name);
  if (seg) {
    if (_len)
      *_len = seg->filesize;
    return seg->fileoff;
  } else
    return 0;
}
# endif
#endif

#ifdef DOS_FILE_SYSTEM
self_exe_t get_self_path(const char *exec_file) XFORM_SKIP_PROC
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

#ifdef SELF_EXE_WINDOWS_AS_UTF8
  {
    char *r;
    int len;
    len = WideCharToMultiByte(CP_UTF8, 0, path, -1, NULL, 0, NULL, NULL);
    r = malloc(len);
    len = WideCharToMultiByte(CP_UTF8, 0, path, -1, r, len, NULL, NULL);
    free(path);
    return r;
  }
#else
  return path;
#endif
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(USE_GENERIC_GET_SELF_PATH) || defined(USE_EXE_LOOKUP_VIA_PATH)

/* Get executable path via argv[0] and the `PATH` environment variable */

static int has_slash(const char *s) XFORM_SKIP_PROC
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
}

static char *do_path_append(const char *s1, int l1, const char *s2) XFORM_SKIP_PROC
{
  int l2;
  char *s;

  l2 = strlen(s2);

  s  = (char *)malloc(l1 + l2 + 2);

  memcpy(s, s1, l1);
  if (s[l1 - 1] != '/') {
    s[l1++] = '/';
  }

  memcpy(s + l1, s2, l2);
  s[l1 + l2] = 0;

  return s;
}

static char *path_append(const char *s1, const char *s2) XFORM_SKIP_PROC
{
  return do_path_append(s1, strlen(s1), s2);
}

static char *copy_string(const char *s1) XFORM_SKIP_PROC
{
  int l1;
  char *s;

  if (!s1) return NULL;

  l1 = strlen(s1);

  s  = (char *)malloc(l1 + 1);

  memcpy(s, s1, l1 + 1);

  return s;
}

static int executable_exists(const char *path) XFORM_SKIP_PROC
{
  return (access(path, X_OK) == 0);
}

static char *get_current_directory() XFORM_SKIP_PROC
{
  char *dir;

  dir = getcwd(NULL, 0);

  if (dir == NULL) {
    /* Probably an old system where you have to allocate space yourself */
    char *s;
    int len = 256;

    s = malloc(len);
    while (1) {
      dir = getcwd(s, len);
      if (dir != NULL)
        break;
      if (errno == ERANGE) {
        free(s);
        len *= 2;
        s = malloc(len);
      } else
        break;
    }

    if (dir == NULL) {
      /* Still failed, so give up with "." as the path */
      s[0] = '.';
      s[1] = 0;
      dir = s;
    }
  }

  return dir;
}

static char *lookup_exe_via_path(const char *exec_file) XFORM_SKIP_PROC
{
  if (exec_file[0] == '/') {
    /* Absolute path */
    return copy_string(exec_file);
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    char *dir, *r;
    dir = get_current_directory();
    r = path_append(dir, exec_file);
    free(dir);
    return r;
  } else {
    /* We have to find the executable by searching PATH: */
    char *path = copy_string(getenv("PATH")), *p, *m, *saved_path = path;
    int more;

    if (!path) {
      path = "";
    }

    while (1) {
      /* Try each element of path: */
      for (p = path; *p && (*p != ':'); p++) { }
      if (*p) {
	*p = 0;
	more = 1;
      } else
	more = 0;

      if (!*path)
	break;

      m = path_append(path, exec_file);

      if (executable_exists(m)) {
	if (m[0] != '/') {
          char *old_m = m;
	  m = path_append(get_current_directory(), m);
          free(old_m);
        }
        if (saved_path) free(saved_path);
	return m;
      }
      free(m);

      if (more)
	path = p + 1;
      else
	break;
    }

    if (saved_path) free(saved_path);

    return copy_string(exec_file);
  }
}
#endif

#ifdef USE_GENERIC_GET_SELF_PATH
# ifndef GENERIC_GET_SELF_PATH_NAME
#  define GENERIC_GET_SELF_PATH_NAME get_self_path
# endif
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file)
{
  return lookup_exe_via_path(exec_file);
}
#endif

#if !defined(OS_X) && !defined(DOS_FILE_SYSTEM) && !defined(SELF_EXE_NO_EXTRAS)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

typedef unsigned short ELF__Half;
typedef unsigned int ELF__Word;
typedef unsigned long ELF__Xword;
typedef unsigned long ELF__Addr;
typedef unsigned long ELF__Off;

typedef struct { 
  unsigned char e_ident[16]; 
  ELF__Half e_type; 
  ELF__Half e_machine; 
  ELF__Word e_version; 
  ELF__Addr e_entry; 
  ELF__Off e_phoff; 
  ELF__Off e_shoff; 
  ELF__Word e_flags; 
  ELF__Half e_ehsize; 
  ELF__Half e_phentsize; 
  ELF__Half e_phnum; 
  ELF__Half e_shentsize; 
  ELF__Half e_shnum;
  ELF__Half e_shstrndx;
} ELF__Header;

typedef struct
{
  ELF__Word sh_name;
  ELF__Word sh_type;
  ELF__Xword sh_flags;
  ELF__Addr sh_addr;
  ELF__Off sh_offset;
  ELF__Xword sh_size;
  ELF__Word sh_link;
  ELF__Word sh_info;
  ELF__Xword sh_addralign;
  ELF__Xword sh_entsize;
} Elf__Shdr;

static int find_elf_section_offset(const char *filename,
                                   const char *name,
                                   int *_start, int *_end) XFORM_SKIP_PROC
{
  int fd, i;
  ELF__Header e;
  Elf__Shdr s;
  char *strs;

  fd = open(filename, O_RDONLY, 0);
  if (fd == -1) return 0;

  if (read(fd, &e, sizeof(e)) == sizeof(e)) {
    if ((e.e_ident[0] == 0x7F)
	&& (e.e_ident[1] == 'E')
	&& (e.e_ident[2] == 'L')
	&& (e.e_ident[3] == 'F')) {

      lseek(fd, e.e_shoff + (e.e_shstrndx * e.e_shentsize), SEEK_SET);
      if (read(fd, &s, sizeof(s)) != sizeof(s)) {
	close(fd);
	return 0;
      }

      strs = (char *)malloc(s.sh_size);
      lseek(fd, s.sh_offset, SEEK_SET);
      if (read(fd, strs, s.sh_size) != s.sh_size) {
	close(fd);
        free(strs);
	return 0;
      }

      for (i = 0; i < e.e_shnum; i++) {
	lseek(fd, e.e_shoff + (i * e.e_shentsize), SEEK_SET);
	if (read(fd, &s, sizeof(s)) != sizeof(s)) {
	  close(fd);
	  return 0;
	}
	if (!strcmp(strs + s.sh_name, name)) {
	  *_start = s.sh_offset;
          *_end = s.sh_offset + s.sh_size;
	  close(fd);
          free(strs);
	  return 1;
	}
      }

      free(strs);
    }
  }

  close(fd);
  return 0;
}

#endif
