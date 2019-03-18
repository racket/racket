#ifndef WIN32
# include <unistd.h>
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef WIN32
# include <windows.h>
# define DOS_FILE_SYSTEM
static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int dest_len, int get_utf16);
#endif
#define BOOT_EXTERN extern
#include "boot.h"

#define MZ_CHEZ_SCHEME
#ifndef INITIAL_BIN_TYPE
# define INITIAL_BIN_TYPE "zi"
#endif
#ifndef RACKET_IS_GUI
# define RACKET_IS_GUI 0
#endif

#ifndef CS_COMPILED_SUBDIR
# define CS_COMPILED_SUBDIR 1
#endif

#define XFORM_SKIP_PROC /* empty */

#include "../../start/config.inc"

#ifdef WIN32
typedef void *(*scheme_dll_open_proc)(const char *name, int as_global);
typedef void *(*scheme_dll_find_object_proc)(void *h, const char *name);
static scheme_dll_open_proc embedded_dll_open;
static scheme_dll_find_object_proc scheme_dll_find_object;
static void scheme_set_dll_procs(scheme_dll_open_proc open, scheme_dll_find_object_proc find)
{
  embedded_dll_open = open;
  scheme_dll_find_object = find;
}
# include "../../start/embedded_dll.inc"
#else
# define embedded_dll_open NULL
# define scheme_dll_find_object NULL
#endif

char *boot_file_data = "BooT FilE OffsetS:xxxxyyyyyzzzz";
static int boot_file_offset = 18;

#define USE_GENERIC_GET_SELF_PATH

#ifdef OS_X
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
#endif

#if defined(__linux__)
# include <errno.h>
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


#ifdef ELF_FIND_BOOT_SECTION
# include <elf.h>
# include <fcntl.h>

static long find_boot_section(char *me)
{
  int fd, i;
  Elf64_Ehdr e;
  Elf64_Shdr s;
  char *strs;

  fd = open(me, O_RDONLY, 0);
  if (fd == -1) return 0;

  if (read(fd, &e, sizeof(e)) == sizeof(e)) {
    lseek(fd, e.e_shoff + (e.e_shstrndx * e.e_shentsize), SEEK_SET);
    if (read(fd, &s, sizeof(s)) != sizeof(s)) {
      close(fd);
      return 0;
    }

    strs = (char *)malloc(s.sh_size);
    lseek(fd, s.sh_offset, SEEK_SET);
    if (read(fd, strs, s.sh_size) != s.sh_size) {
      close(fd);
      return 0;
    }

    for (i = 0; i < e.e_shnum; i++) {
      lseek(fd, e.e_shoff + (i * e.e_shentsize), SEEK_SET);
      if (read(fd, &s, sizeof(s)) != sizeof(s)) {
        close(fd);
        return 0;
      }
      if (!strcmp(strs + s.sh_name, ".rackboot")) {
        close(fd);
        return s.sh_offset;
      }
    }
  }

  close(fd);
  return 0;
}
#endif

#ifdef WIN32
static char *string_to_utf8(wchar_t *p)
{
  char *r;
  int len;

  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, NULL, 0, NULL, NULL);
  r = malloc(len);
  len = WideCharToMultiByte(CP_UTF8, 0, p, -1, r, len, NULL, NULL);

  return r;
}

static int scheme_utf8_encode(unsigned int *path, int zero_offset, int len,
			      char *dest, int offset, int get_utf16)
{
  int dest_len = 0;
  if (dest) {
    dest_len = WideCharToMultiByte(CP_UTF8, 0, (wchar_t *)path, len, NULL, 0, NULL, NULL);
  }
  return WideCharToMultiByte(CP_UTF8, 0, (wchar_t *)path, len, dest, dest_len, NULL, NULL);
}

# include "../start/parse_cmdl.inc"

# undef USE_GENERIC_GET_SELF_PATH
#endif

#ifdef USE_GENERIC_GET_SELF_PATH
/* Get executable path via argv[0] and the `PATH` encironment variable */

static int has_slash(char *s)
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
}

static char *do_path_append(char *s1, int l1, char *s2)
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

static char *path_append(char *s1, char *s2)
{
  return do_path_append(s1, strlen(s1), s2);
}

static char *copy_string(char *s1)
{
  int l1;
  char *s;

  if (!s1) return NULL;

  l1 = strlen(s1);

  s  = (char *)malloc(l1 + 1);

  memcpy(s, s1, l1 + 1);

  return s;
}

static int executable_exists(char *path)
{
  return (access(path, X_OK) == 0);
}

static char *get_self_path(char *exec_file)
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

#ifdef NO_GET_SEGMENT_OFFSET
static long get_segment_offset()
{
  return 0;
}
#endif

#ifndef WIN32
static void *extract_dlldir()
{
  return NULL;
}
#endif

#ifndef do_pre_filter_cmdline_arguments
# define do_pre_filter_cmdline_arguments(argc, argv) /* empty */
#endif

static int bytes_main(int argc, char **argv,
		      /* for Windows and X11 GUI modes */
		      int wm_is_gracket_or_x11_arg_count, char *gracket_guid_or_x11_args)
{
  char *boot_exe, *exec_file = argv[0], *run_file = NULL;
  int pos1, pos2, pos3;
  long boot_offset;
  long segment_offset;
#ifdef WIN32
  wchar_t *dll_path;
  racket_boot_t racket_boot_p;
  long boot_rsrc_offset = 0;
#endif
  
  do_pre_filter_cmdline_arguments(&argc, &argv);

  if (argc) {
    argc--;
    argv++;
  }

#ifdef WIN32
  parse_embedded_dlls();
  register_embedded_dll_hooks();
  if (embedded_dll_open) {
    void *dll;
    dll = embedded_dll_open("libracketcsxxxxxxx.dll", 1);
    boot_rsrc_offset = in_memory_get_offset("libracketcsxxxxxxx.dll");
    racket_boot_p = (racket_boot_t)scheme_dll_find_object(dll, "racket_boot");
    dll_path = get_self_executable_path();
  } else {
    HMODULE dll;
    dll_path = load_delayed_dll_x(NULL, "libracketcsxxxxxxx.dll", &dll);
    racket_boot_p = (racket_boot_t)GetProcAddress(dll, "racket_boot");
  }
  boot_exe = string_to_utf8(dll_path);
# define racket_boot racket_boot_p
#else
  boot_exe = get_self_path(exec_file);
#endif

  extract_built_in_arguments(&exec_file, &run_file, &argc, &argv);
  if (!run_file)
    run_file = exec_file;

  segment_offset = get_segment_offset();

  memcpy(&pos1, boot_file_data + boot_file_offset, sizeof(pos1));
  memcpy(&pos2, boot_file_data + boot_file_offset + 4, sizeof(pos2));
  memcpy(&pos3, boot_file_data + boot_file_offset + 8, sizeof(pos2));

#ifdef ELF_FIND_BOOT_SECTION
  boot_offset = find_boot_section(boot_exe);
#elif WIN32
  boot_offset = find_resource_offset(dll_path, 259, boot_rsrc_offset);
#else
  boot_offset = 0;
#endif

  pos1 += boot_offset;
  pos2 += boot_offset;
  pos3 += boot_offset;

  racket_boot(argc, argv, exec_file, run_file,
	      boot_exe, segment_offset,
              extract_coldir(), extract_configdir(), extract_dlldir(),
              pos1, pos2, pos3,
              CS_COMPILED_SUBDIR, RACKET_IS_GUI,
	      wm_is_gracket_or_x11_arg_count, gracket_guid_or_x11_args,
	      embedded_dll_open, scheme_dll_find_object);
  
  return 0;
}

#if defined(WIN32) && (defined(CHECK_SINGLE_INSTANCE) || defined(__MINGW32__))
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int argc;
  char **argv;
  char *normalized_path;
  int wm = 0;
  char *guid = "";

  argv = cmdline_to_argv(&argc, &normalized_path);

#ifdef CHECK_SINGLE_INSTANCE
  if (CheckSingleInstance(normalized_path, argv))
    return 0;
  wm = wm_is_gracket;
  guid = GRACKET_GUID;
#endif
  
  return bytes_main(argc, argv, wm, guid);
}
#elif defined(WIN32)
int wmain(int argc, wchar_t **wargv)
{
  int i;
  char **argv = malloc(argc * sizeof(char*));

  for (i = 0; i < argc; i++) {
    argv[i] = string_to_utf8(wargv[i]);
  }

  return bytes_main(argc, argv, 0, "");
}
#else
static int x11_arg_count = 0;
static char *x11_args = "0";

int main(int argc, char **argv) {
  return bytes_main(argc, argv, x11_arg_count, x11_args);
}
#endif
