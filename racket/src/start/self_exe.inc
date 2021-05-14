/* Used by "ustart.c" and "config.inc".

   Defines self_exe_t get_self_path(char *exec_file),
   which takes argv[0] and returns an improved representation of
   the containing executable. At worst, on Unix, uses `PATH` to
   convert `exec_file` into a path.

   On Mac (even XonX), `find_mach_o_segment` is also defined.

   On Unix (not Mac OS, not even XonX), `find_elf_section`
   is also defined. 

   If USE_EXE_LOOKUP_VIA_PATH, also: `lookup_exe_via_path`,
   `path_append`, and `do_path_append`.
*/

#define USE_GENERIC_GET_SELF_PATH

#ifdef DOS_FILE_SYSTEM
typedef wchar_t *self_exe_t;
#else
typedef char *self_exe_t;
#endif

#if defined(__linux__)
# include <errno.h>
# include <unistd.h>
static char *get_self_path(char *exec_file)
{
  char buf[256], *s = buf;
  ssize_t len, blen = sizeof(buf);

  while (1) {
    len = readlink("/proc/self/exe", s, blen-1);
    if (len == (blen-1)) {
      if (s != buf) free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
    } else
      break;
  }
  buf[len] = 0;
  return strdup(buf);
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__)
# include <sys/sysctl.h>
# include <errno.h>
static char *get_self_path(char *exec_file)
{
  int mib[4];
  char *s;
  size_t len;
  int r;

  mib[0] = CTL_KERN;
#if defined(__NetBSD__)
  mib[1] = KERN_PROC_ARGS;
  mib[2] = getpid();
  mib[3] = KERN_PROC_PATHNAME;
#else
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_PATHNAME;
  mib[3] = -1;
#endif

  r = sysctl(mib, 4, NULL, &len, NULL, 0);
  if (r < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
  }
  s = malloc(len);
  r = sysctl(mib, 4, s, &len, NULL, 0);
  if (r < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
  }
  return s;
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(__APPLE__) && defined(__MACH__)
# include <mach-o/getsect.h>
# include <mach-o/dyld.h>
static char *get_self_path(char *exec_file)
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
    fprintf(stderr, "failed to get self\n");
    exit(1);
  }
}
# undef USE_GENERIC_GET_SELF_PATH

static long find_mach_o_segment(const char *name, long *_len)
{
# if defined(__x86_64__) || defined(__arm64__)
  const struct segment_command_64 *seg;
# else
  const struct segment_command *seg;
#endif
  seg = getsegbyname(name);
  if (seg) {
    if (_len)
      *_len = seg->filesize;
    return seg->fileoff;
  } else
    return 0;
}
#endif

#ifdef DOS_FILE_SYSTEM
/* used outside this file: */
wchar_t *get_self_path(char *exec_file) XFORM_SKIP_PROC
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
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(USE_GENERIC_GET_SELF_PATH) || defined(USE_EXE_LOOKUP_VIA_PATH)

/* Get executable path via argv[0] and the `PATH` environment variable */

static int has_slash(char *s) XFORM_SKIP_PROC
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
}

static char *do_path_append(char *s1, int l1, char *s2) XFORM_SKIP_PROC
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

static char *path_append(char *s1, char *s2) XFORM_SKIP_PROC
{
  return do_path_append(s1, strlen(s1), s2);
}

static char *copy_string(char *s1) XFORM_SKIP_PROC
{
  int l1;
  char *s;

  if (!s1) return NULL;

  l1 = strlen(s1);

  s  = (char *)malloc(l1 + 1);

  memcpy(s, s1, l1 + 1);

  return s;
}

static int executable_exists(char *path) XFORM_SKIP_PROC
{
  return (access(path, X_OK) == 0);
}

static char *lookup_exe_via_path(char *exec_file) XFORM_SKIP_PROC
{
  if (exec_file[0] == '/') {
    /* Absolute path */
    return exec_file;
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    char *buf;
    long buflen = 4096;
    buf = (char *)malloc(buflen);
    return path_append(getcwd(buf, buflen), exec_file);
  } else {
    /* We have to find the executable by searching PATH: */
    char *path = copy_string(getenv("PATH")), *p, *m;
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
	if (m[0] != '/')
	  m = path_append(getcwd(NULL, 0), m);
	return m;
      }
      free(m);

      if (more)
	path = p + 1;
      else
	break;
    }

    return exec_file;
  }
}
#endif

#ifdef USE_GENERIC_GET_SELF_PATH
static char *get_self_path(char *exec_file)
{
  return lookup_exe_via_path(exec_file);
}
#endif

#if !defined(OS_X) && !defined(DOS_FILE_SYSTEM)

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
